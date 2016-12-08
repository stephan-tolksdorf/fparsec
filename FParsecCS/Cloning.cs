// Copyright (c) Stephan Tolksdorf 2010-2011
// License: Simplified BSD License. See accompanying documentation.

#if !LOW_TRUST

using System;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.Serialization;
using System.Diagnostics;
using System.Reflection.Emit;

namespace FParsec.Cloning {

// The classes in this namespace provide a cloning service based on the serialization API.

// Capturing the state of an object and/or cloning it with this API is often at least an
// order of magnitude faster than doing the same with the BinaryFormatter and a MemoryStream
// (ignoring some initial setup costs and the JITing time).

// Some implementation details:
//
// The serialization API of the BCL (as supported by the BinaryFormatter) spans several
// interfaces, classes and attributes under the System.Runtime.Serialization namespace.
// Unfortunately the publicly available API documentation provided by Microsoft does not
// sufficiently cover certain important details of the serialization API behaviour.
//
// For example, the documentation only vaguely discusses whether serialization events
// like OnDeserialized are invoked in a certain order on multiple objects in a graph.
// It seems that many API users intuitively expect a certain ordering, at least in
// simple cases, but it is also clear that an ordering can not be guaranteed in all
// cases (e.g. in the case of a cyclic object graph).
//
// The .NET BinaryFormatter seems to attempt to invoke the deserialization events
// on dependent objects first, but its behaviour is inconsistent and in some situations
// arguably buggy. The following bug report discusses these issues in more detail:
// https://connect.microsoft.com/VisualStudio/feedback/details/549277
//
// For the sake of compatibility with the .NET BinaryFormatter we try to mimic the
// basic principles of its behaviour. However, we certainly do not try to copy every
// bug or inconsistency.
//
// In order to be on the safe side we sort the serialized object graph topologically
// if it contains objects implementing an OnDeserialized handler or the ISerializable
// or IObjectReferenence interfaces. Since the object graph can contain cycles, we
// first identify strongly connected components using a variant of Tarjan's algorithm.
// Any OnDeserializing handler, deserialization constructor, OnDeserialized handler or
// IObjectReference.GetRealObject method (in that order) is then invoked in the
// topological order starting with the most dependent objects. Objects in a strongly
// connected component (with more than 1 object) are processed in the reverse order
// in which the objects in the component where discovered during a depth-first search
// of the serialized object graph. In a first pass the OnDeserializing handlers and
// deserialization constructors of the objects in the component are invoked. Any
// OnDeserialized handlers are then invoked in a second pass.
// OnSerializing handlers are invoked immediately before an object is serialized.
// OnSerialized handlers are invoked in an undefined order at the end of the
// serialization job (not immediately after an object's subgraph has been serialized).
//
// We only allow an object implementing IObjectReference in a cycle of the serialized
// object graph under the following conditions (which are more restrictive than
// what the .NET BinaryFormatter enforces):
// - There may only be 1 object implementing IObjectReference in a cycle.
// - The type implementing IObjectReference must not be a value type.
// - All objects containing references to the IObjectReference object must have reference types.
// - The type implementing IObjectReference must not have any OnDeserialized handler.
// - There must not be any other object in the cycle implementing ISerializable.
//
// Similar to the .NET BinaryFormatter we delay all IDeserializationCallbacks until
// the end of the deserialization of the complete object graph (not just the relevant
// subgraph). As explained in the referenced Connect bug report this behaviour has some
// severe consequences for the usefulness of IDeserializationCallbacks and the
// composability of the whole serialization API. However, for compatibility we really
// have to stick to Microsoft's design, even if in our case it would actually
// be simpler to invoke the callbacks in topological order as soon as an object's
// subgraph (and its strongly connected component) is completely deserialized.
//
// If the serialized object graph contains unboxed value type instances, any event
// handlers are invoked on boxed copies as follows:
// OnSerializing and OnSerialized handlers are not called on the original value type
// instance (which can be a field or an array element), but on a boxed copy of the
// instance. Thus, if the handler mutates the instance, the changes do not show up in the
// object graph that was serialized, though changes made by OnSerializing (but not
// OnSerialized) will show up in the deserialized object graph. This behaviour
// simplifies the implementation and is in accordance with the behaviour of the
// .NET BinaryFormatter.
// OnDeserializing and OnDeserialized handlers are invoked on a boxed value type instance
// too, but this time any changes show up in the deserialized object graph, because
// the boxed instance is copied into the deserialized object graph after the
// OnDeserialized event (at least if the instance is not part of an object cycle).
// This deviates from the BinaryFormatter behaviour in that
// the BinaryFormatter seems to copy the boxed instance into the deserialized object
// graph before the OnDeserialized event. However, since mutating the instance
// in an OnDeserialized handler has no effect when using the BinaryFormatter,
// hopefully no one causes an incompatibility with this implementation by actually trying
// to mutate the instance. (Note that mutable structs with serialization event handlers
// are extremely rare anyway).
// An IDeserializationCallback.OnDeserialization handler is invoked on the boxed instance
// after it has been copied into the deserialized object graph (and then is not copied
// again), so any changes won't show up in the deserialized unboxed value type instance
// (this holds for both the .NET BinaryFormatter and this implementation).


/// <summary>Contains the serialized state of on object.</summary>
public abstract class CloneImage {
    /// <summary>Deserializes the object state into a new object.</summary>
    public abstract object CreateClone();

    internal CloneImage() {}
}

public abstract class Cloner {
    // public interface
    public readonly Type Type;

    /// <summary>Returns a cloner for the given <em>run-time</em> type.</summary>
    /// <param name="type">The run-time type of the objects to clone. The type must be serializable.</param>
    public static Cloner Create(Type type) {
        lock (Cache) return CreateWithoutLock(type);
    }

    /// <summary>Copies the given object using the serialization API.</summary>
    /// <param name="instance">The object to clone. instance.GetType() must equal the Type the Cloner was created for.</param>
    public object Clone(object instance) {
        return CaptureImage(instance, false).CreateClone();
    }

    /// <summary>Returns an image of the given object instance.</summary>
    /// <param name="instance">The object to capture an image of.</param>
    public CloneImage CaptureImage(object instance) {
        return CaptureImage(instance, true);
    }

    // internal/protected interface

    private readonly CloneEventHandlers EventHandlers;

    private Cloner(Type type, CloneEventHandlers eventHandlers) { Type = type; EventHandlers = eventHandlers; }

    internal abstract State CaptureShallowStateAndEnqueueNestedState(object value, CaptureContext captureContext);

    internal sealed class CaptureContext {
        public readonly bool IsReturnedToUser;

        public CaptureContext(bool stateIsReturnedToUser) {
            IsReturnedToUser = stateIsReturnedToUser;
        }

        // currently uses a static queue, but could easily be rewritten to use an instance queue
        public int GetObjectIndex(object instance, Cloner cloner) {
            Debug.Assert(instance.GetType() == cloner.Type);
            int objectIndex;
            if (!ObjectIndices.TryGetValue(instance, out objectIndex)) {
                objectIndex = ObjectIndices.Count;
                ObjectIndices.Add(instance, objectIndex);
                var item = new WorkItem{Cloner = cloner, Instance = instance};
                WorkQueue.Enqueue(item);
            }
            return objectIndex;
        }
    }

    // internal interface

    internal abstract class State {
        /// <summary>May be null.</summary>
        public readonly CloneEventHandlers EventHandlers;

        /// <summary>Indices of nested objects in the object graph. May be null.</summary>
        public readonly int[] ObjectIndices;

        /// <summary>May be null.</summary>
        public int[] StronglyConnectedComponent;

        public abstract Type Type { get; }
        public abstract object CreateUninitializedObject();
        public abstract void WriteToUninitializedObject(object instance, object[] objectGraph);

        public State(CloneEventHandlers eventHandlers, int[] objectIndices) {
            EventHandlers = eventHandlers;
            ObjectIndices = objectIndices;
        }

        private State() {}
        public static readonly State Dummy = new DummyState();
        private sealed class DummyState : State {
            public override Type Type { get {
                throw new NotImplementedException();
            } }

            public override object CreateUninitializedObject() {
               throw new NotImplementedException();
            }

            public override void WriteToUninitializedObject(object instance, object[] objectGraph) {
                throw new NotImplementedException();
            }
        }
    }

    private static readonly StreamingContext StreamingContext = new StreamingContext(StreamingContextStates.Clone);
    private static readonly FormatterConverter FormatterConverter = new FormatterConverter();

    private static readonly Func<object, object> CloneMemberwise = CreateMemberwiseCloneDelegate();
    private static Func<object, object> CreateMemberwiseCloneDelegate() {
        var dynamicMethod = new DynamicMethod("InvokeMemberwiseClone", typeof(object), new Type[]{typeof(object)}, true);
        var ilg = dynamicMethod.GetILGenerator();
        ilg.Emit(OpCodes.Ldarg_0);
        var method = typeof(object).GetMethod("MemberwiseClone", BindingFlags.NonPublic | BindingFlags.Instance);
        ilg.EmitCall(OpCodes.Call, method, null); // non-virtual call
        ilg.Emit(OpCodes.Ret);
        return (Func<object, object>)dynamicMethod.CreateDelegate(typeof(Func<object, object>));
    }

    // private data and methods

    // Cache serves as the synchronization root for the Create and CaptureImage methods
    private static readonly Dictionary<Type, Cloner> Cache = new Dictionary<Type, Cloner>();

    private static Cloner CreateWithoutLock(Type type) {
        Cloner cloner;
        if (Cache.TryGetValue(type, out cloner)) return cloner;

        if (!type.IsSerializable)
            throw new SerializationException("The type '" + type.ToString() + "' is not marked as serializable.");

        if (!type.IsArray) {
            var eventHandlers = CloneEventHandlers.Create(type);
            if (eventHandlers != null && (eventHandlers.Events & CloneEvents.ISerializable) != 0) {
                cloner = new CustomSerializationCloner(type, eventHandlers);
            } else {
                bool typeIsBlittable;
                var fields = GetSerializedFields(type, out typeIsBlittable);
                if (typeIsBlittable && (eventHandlers == null || (eventHandlers.Events & CloneEvents.OnDeserializing) == 0))
                    cloner = new BlittableCloner(type, eventHandlers, fields);
                else
                    cloner = new NativeSerializationCloner(type, eventHandlers, fields);
            }
        } else { // array
            var elementType = type.GetElementType();
            if (elementType.IsPrimitive || elementType == typeof(string)) {
                cloner = new BlittableCloner(type, null, new FieldInfo[0]);
            } else {
                var elementCloner = CreateWithoutLock(elementType);
                if (elementType.IsValueType && elementCloner is BlittableCloner)
                    cloner = new BlittableCloner(type, null, new FieldInfo[0]);
                else if (type.GetArrayRank() == 1)
                    cloner = new Rank1ArrayCloner(type, elementCloner);
                else
                    cloner = new RankNArrayCloner(type, elementCloner);
            }
        }

        Cache.Add(type, cloner);
        return cloner;
    }

    // for optimization purposes CaptureImage uses some static queues

    private sealed class PhyiscalEqualityObjectComparer : System.Collections.Generic.EqualityComparer<object> {
        public override bool Equals(object x, object y) { return x == y; }
        public override int GetHashCode(object obj) {
            return System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(obj);
        }
    }

    private static readonly Dictionary<object, int> ObjectIndices = new Dictionary<object, int>(new PhyiscalEqualityObjectComparer());
    private static readonly List<State> States = new List<State>();
    private static readonly Queue<WorkItem> WorkQueue = new Queue<WorkItem>();
    private static readonly List<OnSerializedListItem> OnSerializedList = new List<OnSerializedListItem>();
    private static readonly List<int> ObjectReferenceList = new List<int>();

    private struct WorkItem {
        public Cloner Cloner;
        public object Instance;

        public WorkItem(Cloner cloner, object instance) {
            Cloner = cloner;
            Instance = instance;
        }
    }

    private struct OnSerializedListItem {
        public CloneEventHandlers EventHandlers;
        public object Instance;

        public OnSerializedListItem(CloneEventHandlers cloneEventHandlers, object instance) {
            EventHandlers = cloneEventHandlers;
            Instance = instance;
        }
    }

    private static bool Contains(int[] arrayOrNull, int element) {
        if (arrayOrNull != null) {
            foreach (var e in arrayOrNull)
                if (e == element) return true;
        }
        return false;
    }

    private CloneImage CaptureImage(object instance, bool imageIsReturnedToUser) {
        if (instance.GetType() != Type)
            throw new ArgumentException("The object instance does not have the run-time type the Cloner was created for.");
        lock (Cache) {
            try {
                bool needSort = false;

                // reserve 0-index spot
                ObjectIndices.Add(State.Dummy, 0);
                States.Add(null);

                var captureInfo = new CaptureContext(imageIsReturnedToUser);

                ObjectIndices.Add(instance, 1);
                WorkQueue.Enqueue(new WorkItem(this, instance));
                int deserializationCallbackCount = 0;
                do {
                    var item = WorkQueue.Dequeue();
                    var cloner = item.Cloner;
                    if (cloner.EventHandlers == null) {
                        States.Add(item.Cloner.CaptureShallowStateAndEnqueueNestedState(item.Instance, captureInfo));
                    } else if (cloner.EventHandlers.Events == CloneEvents.ISerializable) {
                        States.Add(item.Cloner.CaptureShallowStateAndEnqueueNestedState(item.Instance, captureInfo));
                        needSort = true;
                    } else {
                        var eventHandlers = cloner.EventHandlers;
                        if ((eventHandlers.Events & CloneEvents.OnSerializing) != 0)
                            eventHandlers.InvokeOnSerializing(item.Instance, StreamingContext);
                        if ((eventHandlers.Events & CloneEvents.OnSerialized) != 0)
                            OnSerializedList.Add(new OnSerializedListItem(eventHandlers, item.Instance));
                        var state = item.Cloner.CaptureShallowStateAndEnqueueNestedState(item.Instance, captureInfo);
                        States.Add(state);
                        eventHandlers = state.EventHandlers; // may be different from cloner.EventHandlers (for CustomSerializationState)
                        if ((eventHandlers.Events & (  CloneEvents.ISerializable
                                                     | CloneEvents.OnDeserialized
                                                     | CloneEvents.IObjectReference)) != 0) //
                        {
                            needSort = true;
                            if ((eventHandlers.Events & CloneEvents.IObjectReference) != 0)
                                ObjectReferenceList.Add(States.Count - 1);
                        }
                        // unfortunately the BinaryFormatter doesn't guarantee any order for IDeserializationCallbacks
                        if ((eventHandlers.Events & (CloneEvents.IDeserializationCallback)) != 0)
                            ++deserializationCallbackCount;
                    }
                } while (WorkQueue.Count != 0);
                var states = States.ToArray();

                if (OnSerializedList.Count != 0) {
                    foreach (var item in OnSerializedList)
                        item.EventHandlers.InvokeOnSerialized(item.Instance, StreamingContext);
                }

                if (!needSort)
                    return new SimpleImage(states, deserializationCallbackCount);

                int[] order = ComputeTopologicalOrder(states);
                if (ObjectReferenceList.Count != 0) {
                    foreach (var index1 in ObjectReferenceList) {
                        var state1 = states[index1];
                        var scc = state1.StronglyConnectedComponent;
                        if (scc == null) continue;
                        var type1 = state1.Type;
                        if (type1.IsValueType)
                            throw new SerializationException("The serialized object graph contains a cycle that includes a value type object (type: "+ type1.FullName +") implementing IObjectReference.");
                        if ((state1.EventHandlers.Events & CloneEvents.OnDeserialized) != 0)
                            throw new SerializationException("The serialized object graph contains a cycle that includes an object (type: "+ type1.FullName +") implementing IObjectReference and also exposing an OnDeserialized handler.");
                        foreach (var index2 in scc) {
                            if (index2 == index1) continue;
                            var state2 = states[index2];
                            var type2 = state2.Type;
                            if (state2.EventHandlers != null && (state2.EventHandlers.Events & (CloneEvents.ISerializable | CloneEvents.IObjectReference)) != 0) {
                                var msg = String.Format("The serialized object graph contains a cycle that includes an object (type: {0}) implementing IObjectReference and another object (type: {1}) implementing ISerializable and/or IObjectReference .", type1.FullName, type2.FullName);
                                throw new SerializationException(msg);
                            }
                            if (type2.IsValueType && Contains(state2.ObjectIndices, index1)) {
                                var msg = String.Format("The serialized object graph contains a cycle that includes a value type object (type: {0}) referencing an IObjectReference object (type: {1}) in the same cycle.", type2.FullName, type1.FullName);
                                throw new SerializationException(msg);
                            }
                        }
                    }
                }
                return new OrderedImage(states, order, deserializationCallbackCount);
            } finally {
                States.Clear();
                ObjectIndices.Clear();
                if (WorkQueue.Count != 0) WorkQueue.Clear();
                if (OnSerializedList.Count != 0) OnSerializedList.Clear();
                if (ObjectReferenceList.Count != 0) ObjectReferenceList.Clear();
            }
        }
    }

    private sealed class BlittableCloner : Cloner {
        internal readonly FieldInfo[] SerializedFields;

        public BlittableCloner(Type type, CloneEventHandlers eventHandlers, FieldInfo[] serializedFields) : base(type, eventHandlers) {
            Debug.Assert(serializedFields != null);
            SerializedFields = serializedFields;
        }

        internal override State CaptureShallowStateAndEnqueueNestedState(object instance, CaptureContext captureContext) {
            Debug.Assert(Type == instance.GetType());
            if (captureContext.IsReturnedToUser) {
                return new BlittableState(EventHandlers, CloneMemberwise(instance));
            } else {
                return new BlittableState(EventHandlers, instance);
            }
        }
    }

    private sealed class BlittableState : State {
        private object Value;

        public BlittableState(CloneEventHandlers eventHandlers, object value) : base(eventHandlers, null) {
            Value = value;
        }

        public override Type Type { get { return Value.GetType(); } }

        public override object CreateUninitializedObject() {
            return Cloner.CloneMemberwise(Value);
        }

        public override void WriteToUninitializedObject(object instance, object[] objectGraph) { }
    }

    private sealed class Rank1ArrayCloner : Cloner {
        Cloner  PreviousElementCloner;

        public Rank1ArrayCloner(Type type, Cloner elementCloner) : base(type, null) {
            PreviousElementCloner = elementCloner;
        }

        internal override State CaptureShallowStateAndEnqueueNestedState(object instance, CaptureContext captureContext) {
            Debug.Assert(Type == instance.GetType());
            var array = (Array)instance;
            var lowerBound = array.GetLowerBound(0);
            var length = array.Length; // should throw an exception if length > Int32.MaxValue
            if (length == 0) return new BlittableState(null, instance);
            var throwExceptionOnOverflow = checked(lowerBound + length);
            var objectIndices = new int[length];
            var cloner = PreviousElementCloner;
            var previousType = cloner.Type;
            for (int i = 0; i < length; ++i) {
                var value = array.GetValue(lowerBound + i);
                if (value != null) {
                    var type = value.GetType();
                    if (type != previousType) {
                        cloner = CreateWithoutLock(type);
                        previousType = type;
                    }
                    objectIndices[i] = captureContext.GetObjectIndex(value, cloner);
                }
            }
            PreviousElementCloner = cloner;
            return new Rank1ArrayState(Type.GetElementType(), lowerBound, objectIndices);
        }
    }

    private sealed class Rank1ArrayState : State {
        private readonly Type ElementType;
        private readonly int LowerBound;

        public Rank1ArrayState(Type elementType, int lowerBound, int[] objectIndices) : base(null, objectIndices) {
            Debug.Assert(objectIndices != null);
            ElementType = elementType;
            LowerBound = lowerBound;
        }

        public override Type Type { get { return ElementType.MakeArrayType(); } }

        public override object CreateUninitializedObject() {
            if (LowerBound == 0)
                return Array.CreateInstance(ElementType, ObjectIndices.Length);
            else
                return Array.CreateInstance(ElementType, new int[]{ObjectIndices.Length}, new int[]{LowerBound});
        }

        public override void WriteToUninitializedObject(object instance, object[] objectGraph) {
            var array = (Array)instance;
            var objectIndices = ObjectIndices;
            for (int i = 0; i < objectIndices.Length; ++i) {
                var objectIndex = objectIndices[i];
                if (objectIndex == 0) continue;
                array.SetValue(objectGraph[objectIndex], LowerBound + i);
            }
        }
    }

    private sealed class RankNArrayCloner : Cloner {
        Cloner PreviousElementCloner;

        public RankNArrayCloner(Type type, Cloner elementCloner) : base(type, null) {
            PreviousElementCloner = elementCloner;
        }

        internal override State CaptureShallowStateAndEnqueueNestedState(object instance, CaptureContext captureContext) {
            Debug.Assert(Type == instance.GetType());
            var array = (Array)instance;
            var rank = array.Rank;
            var lowerBounds = new int[rank];
            var lengths = new int[rank];
            var ends = new int[rank];
            var numberOfElements = 1;
            for (int d = 0; d < rank; ++d) {
                var lowerBound = array.GetLowerBound(d);
                lowerBounds[d] = lowerBound;
                var length = array.GetLength(d);
                lengths[d] = length;
                ends[d] = checked(lowerBound + length);
                numberOfElements = checked(numberOfElements * length);
            }
            var objectIndices = new int[numberOfElements];
            var cloner = PreviousElementCloner;
            var previousType = cloner.Type;
            var indices = (int[])lowerBounds.Clone();
            for (int i = 0; i < numberOfElements; ++i) {
                var value = array.GetValue(indices);
                if (value != null) {
                    var type = value.GetType();
                    if (type != previousType) {
                        cloner = CreateWithoutLock(type);
                        previousType = type;
                    }
                    objectIndices[i] = captureContext.GetObjectIndex(value, cloner);
                }
                // increment multi-dimensional index
                var d = rank - 1;
                do {
                    if (++indices[d] < ends[d]) break;
                    indices[d] = lowerBounds[d];
                } while (--d >= 0);
            }
            PreviousElementCloner = cloner;
            return new RankNArrayState(Type.GetElementType(), lengths, lowerBounds, ends, objectIndices);
        }
    }

    private sealed class RankNArrayState : State {
        private readonly Type ElementType;
        private readonly int[] Lengths;
        private readonly int[] LowerBounds;
        private readonly int[] Ends;

        public RankNArrayState(Type elementType, int[] lengths, int[] lowerBounds, int[] ends, int[] objectIndices)
               : base(null, objectIndices)
        {
            Debug.Assert(lengths != null && lengths.Length == lowerBounds.Length && lengths.Length == ends.Length && objectIndices != null);
            ElementType = elementType;
            Lengths = lengths;
            LowerBounds = lowerBounds;
            Ends = ends;
        }

        public override Type Type { get { return ElementType.MakeArrayType(Lengths.Length); } }

        public override object CreateUninitializedObject() {
            return Array.CreateInstance(ElementType, Lengths, LowerBounds);
        }

        public override void WriteToUninitializedObject(object instance, object[] objectGraph) {
            var array = (Array)instance;
            var indices = (int[])LowerBounds.Clone();
            foreach (var objectIndex in ObjectIndices) {
                if (objectIndex != 0)
                    array.SetValue(objectGraph[objectIndex], indices);
                // increment multi-dimensional index
                var d = LowerBounds.Length - 1;
                do {
                    if (++indices[d] < Ends[d]) break;
                    indices[d] = LowerBounds[d];
                } while (--d >= 0);
            }
        }
    }

    private sealed class NativeSerializationCloner : Cloner {
        internal readonly FieldInfo[] SerializedFields;
        private readonly Cloner[] Cloners;

        private Func<object, object[]> FieldValuesGetter; // lazily initialized
        internal Action<object, object[], int[], object[]> FieldValuesSetter; // lazily initialized

        public NativeSerializationCloner(Type type, CloneEventHandlers eventHandlers, FieldInfo[] serializedFields) : base(type, eventHandlers) {
            SerializedFields = serializedFields;
            Cloners = new Cloner[serializedFields.Length];
        }

        internal override State CaptureShallowStateAndEnqueueNestedState(object instance, CaptureContext captureContext) {
            Debug.Assert(Type == instance.GetType());
            if (SerializedFields.Length == 0)
                return new NativeSerializationState(this);
            var getter = FieldValuesGetter;
            if (getter == null)
                FieldValuesGetter = getter = CreateFieldValuesGetter(Type, SerializedFields);
            var values = getter(instance); // GetFieldValues(instance, SerializedFields);
            int[] objectIndices = new int[values.Length];
            for (int i = 0; i < values.Length; ++i) {
                var value = values[i];
                if (value == null) continue;
                var type = value.GetType();
                if (type.IsPrimitive || type == typeof(string)) continue;
                values[i] = null;
                var cloner = Cloners[i];
                if (cloner == null || type != cloner.Type) {
                    cloner = CreateWithoutLock(type);
                    Cloners[i] = cloner;
                }
                objectIndices[i] = captureContext.GetObjectIndex(value, cloner);
            }
            return new NativeSerializationState(this, values, objectIndices);
        }
    }

    private sealed class NativeSerializationState : State {
        private readonly NativeSerializationCloner Cloner;
        private readonly object[] Values; // maybe null if object has no fields

        public NativeSerializationState(NativeSerializationCloner cloner)
               : base(cloner.EventHandlers, null)
        {
            Cloner = cloner;
        }

        public NativeSerializationState(NativeSerializationCloner cloner, object[] values, int[] objectIndices)
               : base(cloner.EventHandlers, objectIndices)
        {
            Debug.Assert(cloner != null && values.Length != 0 && values.Length == objectIndices.Length);
            Cloner = cloner;
            Values = values;
        }

        public override Type Type { get { return Cloner.Type; } }

        public override object CreateUninitializedObject() {
            return FormatterServices.GetUninitializedObject(Cloner.Type);
        }

        public override void WriteToUninitializedObject(object instance, object[] objectGraph) {
            if (ObjectIndices == null) return;
            var setter = Cloner.FieldValuesSetter;
            if (setter == null)
                Cloner.FieldValuesSetter = setter = CreateFieldValuesSetter(Cloner.Type, Cloner.SerializedFields);
            setter(instance, Values, ObjectIndices, objectGraph);
        }
    }

    // NativeSerializationProxyState is used by CustomSerializationCloner to store the state of
    // proxy objects which don't implement ISerializable.
    private sealed class NativeSerializationProxyState : State {
        private readonly Type Type_;
        private readonly FieldInfo[] Fields;
        private readonly object[] Values;

        public NativeSerializationProxyState(Type type, CloneEventHandlers eventHandlers)
               : base(eventHandlers, null)
        {
            Type_ = type;
        }

        public NativeSerializationProxyState(Type type, CloneEventHandlers eventHandlers, FieldInfo[] fields, object[] values, int[] objectIndices)
               : base(eventHandlers, objectIndices)
        {
            Debug.Assert(fields.Length == values.Length && values.Length == objectIndices.Length);
            Type_ = type;
            Fields = fields;
            Values = values;
        }

        public override Type Type { get { return Type_; } }

        public override object CreateUninitializedObject() {
            return FormatterServices.GetUninitializedObject(Type_);
        }

        public override void WriteToUninitializedObject(object instance, object[] objectGraph) {
            if (ObjectIndices == null) return;
            // We can't use a NativeSerializationCloner.FieldValuesSetter here
            // because some primitive values might have a type different from the type of the field
            // they are assigned to. FieldInfo.SetValue does some automatic conversions in those
            // cases that the FieldValuesSetter doesn't (e.g. integer type widening).
            for (int i = 0; i < ObjectIndices.Length; ++i) {
                var objectIndex = ObjectIndices[i];
                if (objectIndex == 0) {
                    var value = Values[i];
                    if (value != null) Fields[i].SetValue(instance, value);
                } else {
                    Fields[i].SetValue(instance, objectGraph[objectIndex]);
                }
            }
        }
    }

    private struct CustomSerializationMemberInfo {
        public string Name;
        public Type Type;
        public object Value;
    }

    private sealed class CustomSerializationCloner : Cloner {
        internal readonly ConstructorInfo Constructor;
        internal Action<object, SerializationInfo, StreamingContext> ConstructorCaller; // lazily initalized
        private Cloner PreviousProxyCloner;
        private Cloner[] Cloners;

        private static Type[] SerializableConstructorArgumentTypes = new Type[] {typeof(SerializationInfo), typeof(StreamingContext)};

        public CustomSerializationCloner(Type type,
                                         CloneEventHandlers eventHandlers)
               : base(type, eventHandlers)
        {
            Constructor = type.GetConstructor(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance, null, SerializableConstructorArgumentTypes, null);
            PreviousProxyCloner = this;
        }

        internal override State CaptureShallowStateAndEnqueueNestedState(object instance, CaptureContext captureContext) {
            Debug.Assert(Type == instance.GetType());
            var info = new SerializationInfo(Type, FormatterConverter);
            ((ISerializable)instance).GetObjectData(info, StreamingContext);
            var n = info.MemberCount;
            var members = new CustomSerializationMemberInfo[n];
            var objectIndices = new int[n];
            if (Cloners == null || Cloners.Length != n) Cloners = new Cloner[n];
            var iter = info.GetEnumerator();
            for (int i = 0; iter.MoveNext(); ++i) {
                var entry = iter.Current;
                members[i].Name = entry.Name;
                members[i].Type = entry.ObjectType;
                var value = entry.Value;
                if (value == null) continue;
                Type type = value.GetType();
                if (type.IsPrimitive || type == typeof(string)) {
                    members[i].Value = value;
                    continue;
                }
                var cloner = Cloners[i];
                if (cloner == null || type != cloner.Type) {
                    cloner = CreateWithoutLock(type);
                    Cloners[i] = cloner;
                }
                objectIndices[i] = captureContext.GetObjectIndex(value, cloner);
            }

            Type proxyType;

        #if CLR4
            if (!info.IsFullTypeNameSetExplicit && !info.IsAssemblyNameSetExplicit) {
                proxyType = info.ObjectType;
        #else
            if (info.FullTypeName == Type.FullName && info.AssemblyName == Type.Assembly.FullName) {
                proxyType = Type;
        #endif
            } else {
                try {
                    var assembly = Assembly.Load(info.AssemblyName);
                    proxyType = assembly.GetType(info.FullTypeName, true);
                } catch (Exception e) {
                    var msg = "Can not load the type '" + info.FullTypeName + "' in the assembly '" + info.AssemblyName + "'.";
                    throw new SerializationException(msg, e);
                }
            }

            if (proxyType == Type) {
                if (Constructor == null) throw new SerializationException("The ISerializable type '" + Type.ToString() + "' does not define a proper deserialization constructor.");
                return new CustomSerializationState(this, members, objectIndices);
            }

            Cloner proxyCloner;
            if (proxyType == PreviousProxyCloner.Type) {
                proxyCloner = PreviousProxyCloner;
            } else {
                proxyCloner = CreateWithoutLock(proxyType);
                PreviousProxyCloner = proxyCloner;
            }

            if (proxyType.IsArray) {
                // On .NET a NullReferenceException is thrown on deserialization of an array type proxy.
                throw new SerializationException("The type '" + Type.ToString() + "' uses an array type ('" + proxyType.ToString() +  "') as its serialization proxy type.");
            }

            CustomSerializationCloner csc = proxyCloner as CustomSerializationCloner;
            if (csc != null) {
                if (csc.Constructor == null) throw new SerializationException("The ISerializable type '" + csc.Type.ToString() + "' does not define a proper deserialization constructor.");
                return new CustomSerializationState(csc, members, objectIndices);
            }

            if (n == 0) return new NativeSerializationProxyState(proxyType, proxyCloner.EventHandlers);

            FieldInfo[] proxyFields;
            {
                var nsc = proxyCloner as NativeSerializationCloner;
                if (nsc != null) {
                    proxyFields = nsc.SerializedFields;
                } else {
                    var bc = proxyCloner as BlittableCloner;
                    Debug.Assert(bc != null);
                    proxyFields = bc.SerializedFields;
                }
            }

            // The BinaryFormatter on .NET simply assigns the values in the SerializationInfo
            // to the field with the same name (of the most derived class) in the proxy object.
            // There are no checks whether all fields are assigned values or whether the target has
            // multiple fields with the same name. The types are only checked once the values are
            // assigned to the proxy object fields. Integer types are automatically widened and
            // types are cast to base or interface types if necessary.

            var proxyValues = new object[proxyFields.Length];
            var proxyObjectIndices = new int[proxyFields.Length];
            for (int i = 0; i < n; ++i) {
                var name = members[i].Name;
                for (int j = 0; j < proxyFields.Length; ++j) {
                    if (name == proxyFields[j].Name) {
                        proxyValues[j] = members[i].Value;
                        proxyObjectIndices[j] = objectIndices[i];
                        break;
                    }
                }
            }
            return new NativeSerializationProxyState(proxyType, proxyCloner.EventHandlers, proxyFields, proxyValues, proxyObjectIndices);
        }
    }

    private sealed class CustomSerializationState : State {
        private readonly CustomSerializationCloner Cloner;
        private readonly CustomSerializationMemberInfo[] Members;

        public CustomSerializationState(CustomSerializationCloner cloner,
                                       CustomSerializationMemberInfo[] members,
                                       int[] objectIndices)
               : base(cloner.EventHandlers, objectIndices)
        {
            Cloner = cloner;
            Members = members;
        }

        public override Type Type { get { return Cloner.Type; } }

        public override object CreateUninitializedObject() {
            return FormatterServices.GetUninitializedObject(Cloner.Type);
        }

        public override void WriteToUninitializedObject(object instance, object[] objectGraph) {
            var info = new SerializationInfo(Cloner.Type, FormatterConverter);
            for (int i = 0; i < Members.Length; ++i) {
                var member = Members[i];
                var index = ObjectIndices[i];
                var value = index == 0 ? member.Value : objectGraph[index];
                info.AddValue(member.Name, value, member.Type);
            }
            var constructorCaller = Cloner.ConstructorCaller;
            if (constructorCaller == null)
                Cloner.ConstructorCaller = constructorCaller = CreateISerializableConstructorCaller(Cloner.Constructor);
            constructorCaller(instance, info, StreamingContext);
        }
    }

    private sealed class SimpleImage : CloneImage {
        private readonly Cloner.State[] States;
        private readonly int DeserializationCallbackCount;

        internal SimpleImage(Cloner.State[] states, int deserializationCallbackCount) {
            Debug.Assert(states.Length > 1 && states[0] == null);
            States = states;
            DeserializationCallbackCount = deserializationCallbackCount;
        }

        public override object CreateClone() {
            int callbackIndicesIndex = DeserializationCallbackCount;
            int[] callbackIndices =
                DeserializationCallbackCount == 0 ? null : new int[DeserializationCallbackCount];
            var objects = new object[States.Length];
            // States[0] is null
            for (int i = 1; i < States.Length; ++i)
                objects[i] = States[i].CreateUninitializedObject();
            for (int index = States.Length - 1; index != 0; --index) {
                var state = States[index];
                var instance = objects[index];
                var eventHandlers = state.EventHandlers;
                if (eventHandlers == null) {
                    state.WriteToUninitializedObject(objects[index], objects);
                } else {
                    var events = eventHandlers.Events;
                    Debug.Assert((events & (  CloneEvents.ISerializable
                                            | CloneEvents.OnDeserialized
                                            | CloneEvents.IObjectReference)) == 0);
                    if ((events & CloneEvents.OnDeserializing) != 0)
                        eventHandlers.InvokeOnDeserializing(instance, Cloner.StreamingContext);
                    if ((events & CloneEvents.IDeserializationCallback) != 0)
                        callbackIndices[--callbackIndicesIndex] = index;
                    state.WriteToUninitializedObject(instance, objects);
                }
            }
            if (callbackIndices != null) {
                Debug.Assert(callbackIndicesIndex == 0);
                foreach (var index in callbackIndices)
                    ((IDeserializationCallback)objects[index]).OnDeserialization(null);
            }
            return objects[1];
        }
    }

    private sealed class OrderedImage : CloneImage {
        private readonly Cloner.State[] States;
        private readonly int[] Order;
        private readonly int DeserializationCallbackCount;

        internal OrderedImage(Cloner.State[] states, int[] order, int deserializationCallbackCount) {
            Debug.Assert(states.Length > 1 && states.Length == order.Length && states[0] == null);
            States = states;
            Order = order;
            DeserializationCallbackCount = deserializationCallbackCount;
        }

        public static object GetRealObject(object instance) {
            var or = (IObjectReference)instance;
            instance = or.GetRealObject(Cloner.StreamingContext);
            if (instance != or) {
                or = instance as IObjectReference;
                int i = 0;
                while (or != null) {
                    if (++i == 100) throw new SerializationException("An object's implementation of the IObjectReference interface returned too many nested references to other objects that implement IObjectReference.");
                    instance = or.GetRealObject(Cloner.StreamingContext);
                    if (instance == or) break;
                    or = instance as IObjectReference;
                }
                if (instance == null) throw new SerializationException("An object's IObjectReference.GetRealObject implementation returned null.");
            }
            return instance;
        }

        public override object CreateClone() {
            int callbackIndicesIndex = DeserializationCallbackCount;
            object[] callbackObjects =
                DeserializationCallbackCount == 0 ? null : new object[DeserializationCallbackCount];
            var objects = new object[States.Length];
            for (int i = 1; i < States.Length; ++i)
                objects[i] = States[i].CreateUninitializedObject();
            var delayedOnDeserializedEvents = new List<int>();
            int objectReferenceIndex = 0;
            object objectReference = null;
            int[] lastScc = null;
            for (int i = Order.Length - 1; i != 0; --i) {
                var index = Order[i];
                var state = States[index];
                var scc = state.StronglyConnectedComponent;
                if (scc != lastScc) {
                    lastScc = scc;
                    if (objectReference != null) {
                        ReplaceObjectReferenceInSCCWithRealObject(objectReference, objectReferenceIndex, objects);
                        objectReferenceIndex = 0;
                        objectReference = null;
                    }
                    if (delayedOnDeserializedEvents.Count != 0)
                        InvokeDelayedOnDeserializedEvents(delayedOnDeserializedEvents, objects); // also clears delayedOnDeserializedEvents
                    if (scc != null) {
                        foreach (var idx in scc)  {
                            var handlers = States[idx].EventHandlers;
                            if (handlers != null && (handlers.Events & CloneEvents.IObjectReference) != 0) {
                                objectReferenceIndex = idx;
                                objectReference = objects[idx];
                                objects[idx] = null; // set to null until we call ReplaceObjectReferenceInSCCWithRealObject
                            }
                        }
                    }
                }
                var instance = objects[index];
                var eventHandlers = state.EventHandlers;
                if (eventHandlers == null) {
                    state.WriteToUninitializedObject(instance, objects);
                } else {
                    var events = eventHandlers.Events;
                    if (instance != null) {
                        if ((events & CloneEvents.OnDeserializing) != 0)
                            eventHandlers.InvokeOnDeserializing(instance, Cloner.StreamingContext);
                        state.WriteToUninitializedObject(instance, objects);
                        if ((events & CloneEvents.OnDeserialized) != 0) {
                            if (scc == null) eventHandlers.InvokeOnDeserialized(instance, Cloner.StreamingContext);
                            else delayedOnDeserializedEvents.Add(index);
                        }
                        if ((events & CloneEvents.IObjectReference) != 0) {
                            Debug.Assert(state.StronglyConnectedComponent == null);
                            objects[index] = GetRealObject(instance);
                        }
                    } else {
                        Debug.Assert(index == objectReferenceIndex);
                    }
                    // It's a pity we have to process the IDeserializationCallback separately
                    // from OnDeserialized events to stay compatible with the .NET BinaryFormatter.
                    if ((events & CloneEvents.IDeserializationCallback) != 0)
                        callbackObjects[--callbackIndicesIndex] = instance ?? objectReference;
                }
            }
            if (objectReference != null)
                ReplaceObjectReferenceInSCCWithRealObject(objectReference, objectReferenceIndex, objects);
            if (delayedOnDeserializedEvents.Count != 0)
                InvokeDelayedOnDeserializedEvents(delayedOnDeserializedEvents, objects);
            if (callbackObjects != null) {
                Debug.Assert(callbackIndicesIndex == 0);
                // We call the callback in in the reverse topological order at the end of
                // deserialization, which is similar to what the BinaryFormatter does, unfortunately.
                foreach (var obj in callbackObjects)
                    ((IDeserializationCallback)obj).OnDeserialization(null);
            }
            return objects[1];
        }

        private void InvokeDelayedOnDeserializedEvents(List<int> indices, object[] objects) {
            foreach (var index in indices) {
                var handlers = States[index].EventHandlers;
                handlers.InvokeOnDeserialized(objects[index], Cloner.StreamingContext);
            }
            indices.Clear();
        }

        private void ReplaceObjectReferenceInSCCWithRealObject(object objectReference, int objectReferenceIndex, object[] objects) {
            var state = States[objectReferenceIndex];
            var eventHandlers = state.EventHandlers;
            var events = eventHandlers.Events;
            if ((events & CloneEvents.OnDeserializing) != 0)
                eventHandlers.InvokeOnDeserializing(objectReference, Cloner.StreamingContext);
            state.WriteToUninitializedObject(objectReference, objects);
            Debug.Assert((events & CloneEvents.OnDeserialized) == 0);
            objects[objectReferenceIndex] = GetRealObject(objectReference);
            // set all references to real object
            foreach (var index2 in state.StronglyConnectedComponent) {
                if (index2 == objectReferenceIndex) continue;
                var state2 = States[index2];
                Debug.Assert(state2.EventHandlers == null || (state2.EventHandlers.Events & (CloneEvents.ISerializable | CloneEvents.IObjectReference)) == 0);
                if (Cloner.Contains(state2.ObjectIndices, objectReferenceIndex)) {
                    Debug.Assert(!state2.Type.IsValueType);
                    state2.WriteToUninitializedObject(objects[index2], objects); // overwrite all fields
                }
            }
        }
    }

    /// <summary>Returns the public and non-public fields of the type (and its base types),
    /// except fields with the NonSerialized attribute. In the returned array fields from
    /// derived types come before fields from base types.</summary>
    internal static FieldInfo[] GetSerializedFields(Type type, out bool typeIsBlittable) {
        Debug.Assert(type.IsSerializable && !type.IsInterface);
        // We need the fields of the most derived type first, but GetFields returns the
        // field in an undefined order, so we have to climb the type hierarchy.
        var fields = type.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.DeclaredOnly);
        bool isBlittable = true;
        int nonSerialized = 0;
        foreach (var f in fields) {
            if (f.IsNotSerialized) ++nonSerialized;
            var ft = f.FieldType;
            if (!ft.IsPrimitive && ft != typeof(string)) {
                if (!ft.IsValueType) isBlittable = false;
                else {
                    bool fIsBlittable;
                    GetSerializedFields(ft, out fIsBlittable);
                    isBlittable &= fIsBlittable;
                }
            }
        }
        int numberOfBases = 0;
        var bt = type.BaseType;
        while (bt != null && bt != typeof(object)) {
            if (!bt.IsSerializable)
                throw new SerializationException(BaseTypeNotSerializableMessage(bt, type));
            ++numberOfBases;
            bt = bt.BaseType;
        }
        if (numberOfBases == 0) {
            if (nonSerialized == 0) {
                typeIsBlittable = isBlittable;
                return fields;
            } else {
                typeIsBlittable = false;
                var serializedFields = new FieldInfo[fields.Length - nonSerialized];
                int i = 0;
                foreach (var f in fields) if (!f.IsNotSerialized) serializedFields[i++] = f;
                return serializedFields;
            }
        } else {
            var baseFieldArrays = new FieldInfo[numberOfBases][];
            bt = type.BaseType;
            for (int i = 0; i < numberOfBases; ++i, bt = bt.BaseType) {
                var baseFields = bt.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.DeclaredOnly);
                foreach (var bf in baseFields) {
                    if (bf.IsNotSerialized) ++nonSerialized;
                    var bft = bf.FieldType;
                    if (!bft.IsPrimitive && bft != typeof(string)) {
                        if (!bft.IsValueType) isBlittable = false;
                        else {
                            bool bfIsBlittable;
                            GetSerializedFields(bft, out bfIsBlittable);
                            isBlittable &= bfIsBlittable;
                        }
                    }
                }
                baseFieldArrays[i] = baseFields;
            }

            typeIsBlittable = nonSerialized == 0 & isBlittable;

            var numberOfSerializedFields = fields.Length - nonSerialized;
            foreach (var baseFields in baseFieldArrays) numberOfSerializedFields += baseFields.Length;

            if (nonSerialized == 0 && numberOfSerializedFields == fields.Length) return fields;

            var combinedFields = new FieldInfo[numberOfSerializedFields];
            if (nonSerialized == 0) {
                int i = 0;
                foreach (var f in fields) combinedFields[i++] = f;
                foreach (var baseFields in baseFieldArrays)
                    foreach (var bf in baseFields) combinedFields[i++] = bf;
            } else {
                int i = 0;
                foreach (var f in fields)
                    if (!f.IsNotSerialized) combinedFields[i++] = f;
                foreach (var baseFields in baseFieldArrays)
                    foreach (var bf in baseFields)
                        if (!bf.IsNotSerialized) combinedFields[i++] = bf;
            }
            return combinedFields;
        }
    }

    internal static string BaseTypeNotSerializableMessage(Type baseType, Type childType) {
        return "The serializable type '" + childType.ToString() + "' has a base type '" + baseType.ToString() + "' that is not serializable.";
    }

    /*
    private static object[] GetFieldValues(object instance, FieldInfo[] fields) {
        var values = new object[fields.Length];
        for (int i = 0; i < fields.Length; ++i) {
            var f = fields[i];
            values[i] = f.GetValue(instance);
        }
        return values;
    }
    */
    internal static Func<object, object[]> CreateFieldValuesGetter(Type type, FieldInfo[] fields) {
        if (fields.Length == 0) throw new ArgumentException("The fields array must be non-empty.");

        var dynamicMethod = new DynamicMethod("FieldValuesGetter",
                                               MethodAttributes.Public | MethodAttributes.Static,
                                               CallingConventions.Standard,
                                               typeof(object[]), new Type[]{typeof(object), typeof(object)},
                                               type, true);
        var ilg = dynamicMethod.GetILGenerator();
        var isValueType = type.IsValueType;

        // arg 0: dummy argument (makes delegate invocation faster)
        // arg 1: (boxed) object instance

        ilg.DeclareLocal(typeof(object[])); // local 0: the returned values array
        ilg.DeclareLocal(typeof(object)); // local 1: temporary object value

        // create the values array
        ilg.Emit(OpCodes.Ldc_I4, fields.Length);
        ilg.Emit(OpCodes.Newarr, typeof(object));
        ilg.Emit(OpCodes.Stloc_0);

        // cast/unbox the object instace
        ilg.Emit(OpCodes.Ldarg_1);
        if (!isValueType)
            ilg.Emit(OpCodes.Castclass, type);
        else
            ilg.Emit(OpCodes.Unbox, type);

        // The unbox IL construction doesn't return a normal managed pointer
        // but a "controlled-mutability" managed pointer. Since there's no way
        // to declare a controlled-mutability managed pointer local and one
        // can't convert such a pointer into a normal managed pointer, we can't
        // store away the pointer for later field accesses. Instead we use
        // OpCodes.Dup to keep the pointer around. Alternatively we could copy
        // the value type instance onto the stack, but that can be costly for
        // large value types.

        for (int i = 0; i < fields.Length; ++i) {
            if (i + 1 != fields.Length) ilg.Emit(OpCodes.Dup);

            var field = fields[i];
            ilg.Emit(OpCodes.Ldfld, field);
            if (field.FieldType.IsValueType)
                ilg.Emit(OpCodes.Box, field.FieldType);
            ilg.Emit(OpCodes.Stloc_1);

            // store object into result array
            ilg.Emit(OpCodes.Ldloc_0);
            ilg.Emit(OpCodes.Ldc_I4, i);
            ilg.Emit(OpCodes.Ldloc_1);
            ilg.Emit(OpCodes.Stelem_Ref);
        }

        ilg.Emit(OpCodes.Ldloc_0);
        ilg.Emit(OpCodes.Ret);

        return (Func<object, object[]>)dynamicMethod.CreateDelegate(typeof(Func<object, object[]>), null);
    }

    /*
    private static void SetFieldValues(FieldInfo[] fields, object instance, object[] values, int[] objectIndices, object[] objectGraph) {
        for (int i = 0; i < objectIndices.Length; ++i) {
            var objectIndex = ObjectIndices[i];
            if (objectIndex == 0)
                fields[i].SetValue(instance, values[i]);
            else
                fields[i].SetValue(instance, objectGraph[objectIndex]);
        }
    }
    */
    internal static Action<object, object[], int[], object[]> CreateFieldValuesSetter(Type type, FieldInfo[] fields) {
        if (fields.Length == 0) throw new ArgumentException("The fields array must be non-empty.");

        // It is important that we use the 8 argument DynamicMethod constructor
        // to associate the method with the type, so that the method is allowed
        // to set readonly (initonly) fields.
        var dynamicMethod = new DynamicMethod("FieldValuesSetter",
                                              MethodAttributes.Public | MethodAttributes.Static, CallingConventions.Standard,
                                              null, new Type[]{typeof(object), typeof(object), typeof(object[]), typeof(int[]),
                                              typeof(object[])},
                                              type, true);
        var ilg = dynamicMethod.GetILGenerator();
        var isValueType = type.IsValueType;

        // arg0: dummy argument (makes delegate invocation faster)
        // arg1: (boxed) object instance
        // arg2: values array
        // arg3: objectIndices array
        // arg4: objectGraph array

        // local 0: object index
        ilg.DeclareLocal(typeof(int));

        ilg.Emit(OpCodes.Ldarg_1);
        if (!isValueType)
            ilg.Emit(OpCodes.Castclass, type);
        else
            ilg.Emit(OpCodes.Unbox, type); // returns a controlled-mutability pointer
                                          // which we can't store in a local...
        for (int i = 0; i < fields.Length; ++i) {
            if (i + 1 != fields.Length) ilg.Emit(OpCodes.Dup); // ... so we use OpCodes.Dup to keep it around

            var field = fields[i];

            // is field value an object in the object graph array?
            ilg.Emit(OpCodes.Ldarg_3);
            ilg.Emit(OpCodes.Ldc_I4, i);
            ilg.Emit(OpCodes.Ldelem, typeof(int));
            ilg.Emit(OpCodes.Stloc_0);
            ilg.Emit(OpCodes.Ldloc_0);
            var label1 = ilg.DefineLabel();
            ilg.Emit(OpCodes.Brtrue, label1);

            // load boxed value
            ilg.Emit(OpCodes.Ldarg_2);
            ilg.Emit(OpCodes.Ldc_I4, i);
            ilg.Emit(OpCodes.Ldelem, typeof(object));
            var label2 = ilg.DefineLabel();
            ilg.Emit(OpCodes.Br, label2);

            // load object graph array
            ilg.MarkLabel(label1);
            ilg.Emit(OpCodes.Ldarg, 4);
            ilg.Emit(OpCodes.Ldloc_0);
            ilg.Emit(OpCodes.Ldelem, typeof(object));

            ilg.MarkLabel(label2);
            // store value into field
            if (field.FieldType != typeof(object))
                ilg.Emit(OpCodes.Unbox_Any, field.FieldType);
            ilg.Emit(OpCodes.Stfld, field);
        }

        ilg.Emit(OpCodes.Ret);

        return (Action<object, object[], int[], object[]>)dynamicMethod.CreateDelegate(typeof(Action<object, object[], int[], object[]>), null);
    }

    internal static Action<object, SerializationInfo, StreamingContext> CreateISerializableConstructorCaller(ConstructorInfo constructor) {
        var type = constructor.DeclaringType;
        var dynamicMethod = new DynamicMethod("SerializableConstructorCaller",
                                              MethodAttributes.Public | MethodAttributes.Static, CallingConventions.Standard,
                                              null, new Type[]{typeof(object), typeof(object), typeof(SerializationInfo), typeof(StreamingContext)},
                                              type, true);
        var ilg = dynamicMethod.GetILGenerator();
        var isValueType = type.IsValueType;
        ilg.Emit(OpCodes.Ldarg_1);
        if (!isValueType)
            ilg.Emit(OpCodes.Castclass, type);
        else
            ilg.Emit(OpCodes.Unbox, type);
        ilg.Emit(OpCodes.Ldarg_2);
        ilg.Emit(OpCodes.Ldarg_3);
        ilg.Emit(OpCodes.Call, constructor);
        ilg.Emit(OpCodes.Ret);
        return (Action<object, SerializationInfo, StreamingContext>)dynamicMethod.CreateDelegate(typeof(Action<object, SerializationInfo, StreamingContext>), null);
    }

    // The following is a non-recursive implementation of David J. Pearce's improved
    // version of Tarjan's algorithm for finding the strongly connected components of
    // a directed graph, see http://homepages.ecs.vuw.ac.nz/~djp/files/P05.pdf
    // The straighforward recursive version is obviously more elegant, but the
    // non-recursive one has the principal advantage of not ending in a stack overflow
    // for large components.
    // (We test this version against the simpler one in CloningTests.fs, of course)
    // Due to the non-recursive implementation we can also exploit that part of
    // what would otherwise be the call stack can be shared with the stack used
    // for holding elements of identified components (see the last paragraph of
    // section 2 in the referenced paper).

    // For optimization purposes we use a static stack, which makes
    // FindStronglyConnectedComponents and ComputeTopologicalOrder not thread-safe.

    private static int[] TopoIndices = new int[8];
    private static void GrowTopoIndices() {
        var newArray = new int[2*TopoIndices.Length];
        TopoIndices.CopyTo(newArray, 0);
        TopoIndices = newArray;
    }
    private static int GrowTopoIndices(int splitIndex) {
        Debug.Assert(splitIndex >= 0 && splitIndex <= TopoIndices.Length);
        int n = TopoIndices.Length;
        var newArray = new int[2*n];
        Array.Copy(TopoIndices, newArray, splitIndex);
        var newSplitIndex = 2*n;
        int d = n - splitIndex;
        if (d != 0) {
            newSplitIndex -= d;
            Array.Copy(TopoIndices, splitIndex, newArray, newSplitIndex, n - splitIndex);
        }
        TopoIndices = newArray;
        return newSplitIndex;
    }

    private static int[] TopoSubIndices = new int[8];
    private static void GrowTopoSubIndices() {
        var newArray = new int[2*TopoSubIndices.Length];
        TopoSubIndices.CopyTo(newArray, 0);
        TopoSubIndices = newArray;
    }

    /// <summary>Fills the Strongly StronglyConnectedComponent fields of the
    /// states passed in the array. Returns an array mapping each state to an
    /// integer component identifier.
    /// </summary>
    /// <param name="states">The object states to traverse. The object with array index
    /// 0 is ignored. All other objects are assumed to be reachable from the object
    /// with array index 1.</param>
    internal static int[] FindStronglyConnectedComponents(State[] states) {
        Debug.Assert(states.Length > 1);
        int[] components = new int[states.Length];
        // The path stack and the component stack are both stored in TopoIndices.
        // The path stack starts at the beginning of TopoIndices, while
        // the component stack starts at the end and progresses in reverse direction.
        int pathStackCount = 0; // number of elements in the path stack
        int componentStackIndex = TopoIndices.Length; // index of element last inserted into component stack
        int counter = 1; // in the paper this variable is called "index"
        int reverseCounter = states.Length - 1; // in the paper this variable is called "C"
        bool root = true;
        int objectIndex = 1; // states[1] is state for the root object, states[0] is null
        int subIndex = 0;
        var subObjectIndices = states[objectIndex].ObjectIndices;
        components[1] = counter;
        ++counter;
        if (subObjectIndices != null) {
            for (;;) {
                while (subIndex < subObjectIndices.Length) {
                    var subObjectIndex = subObjectIndices[subIndex];
                    ++subIndex;
                    if (subObjectIndex == 0) continue;
                    var subObjectComponent = components[subObjectIndex];
                    if (subObjectComponent == 0) {
                        var subSubObjectIndices = states[subObjectIndex].ObjectIndices;
                        if (subSubObjectIndices == null) {
                            components[subObjectIndex] = reverseCounter;
                            --reverseCounter;
                        } else {
                            subObjectIndices = subSubObjectIndices;
                            components[subObjectIndex] = counter;
                            ++counter;
                            TopoIndices[pathStackCount] = objectIndex;
                            TopoSubIndices[pathStackCount] = root ? subIndex : -subIndex;
                            root = true;
                            objectIndex = subObjectIndex;
                            subIndex = 0;
                            ++pathStackCount;
                            if (pathStackCount == componentStackIndex)
                                componentStackIndex = GrowTopoIndices(componentStackIndex);
                            if (pathStackCount == TopoSubIndices.Length)
                                GrowTopoSubIndices();
                            continue;
                        }
                    } else if (subObjectComponent < components[objectIndex]) {
                        components[objectIndex] = subObjectComponent;
                        root = false;
                    }
                }
                if (root) {
                    if (componentStackIndex < TopoIndices.Length) {
                        int component = components[objectIndex];
                        if (components[TopoIndices[componentStackIndex]] >= component) {
                            int next = componentStackIndex + 1;
                            while (next < TopoIndices.Length && components[TopoIndices[next]] >= component) ++next;
                            int d = next - componentStackIndex;
                            var scc = new int[d + 1];
                            for (int i = 0; i < d; ++i) {
                                int idx = TopoIndices[componentStackIndex + i];
                                scc[1 + i] = idx;
                                states[idx].StronglyConnectedComponent = scc;
                                components[idx] = reverseCounter;
                                --counter;
                            }
                            scc[0] = objectIndex;
                            states[objectIndex].StronglyConnectedComponent = scc;
                            componentStackIndex = next;
                        }
                    }
                    components[objectIndex] = reverseCounter;
                    --counter;
                    --reverseCounter;
                    if (pathStackCount == 0) break;
                } else {
                    TopoIndices[--componentStackIndex] = objectIndex;
                    // we never need to grow the TopoIndices array here
                    // because we immediately decrement pathStackCount next
                }
                --pathStackCount;
                int subObjectComponent_ = components[objectIndex];
                objectIndex = TopoIndices[pathStackCount];
                subIndex = TopoSubIndices[pathStackCount];
                if (subIndex > 0) {
                    root = true;
                } else {
                    subIndex = -subIndex;
                    root = false;
                }
                subObjectIndices = states[objectIndex].ObjectIndices;
                if (subObjectComponent_ < components[objectIndex]) {
                    components[objectIndex] = subObjectComponent_;
                    root = false;
                }
            }
        }
        return components;
    }

    private static int[] SccIndexStack = new int[8];
    private static void GrowSccIndexStack() {
        var newStack = new int[2*SccIndexStack.Length];
        SccIndexStack.CopyTo(newStack, 0);
        SccIndexStack = newStack;
    }

    /// <summary>Returns an array with the topologically sorted indices of the states.
    /// In the returned array the indices of states belonging to the same strongly
    /// connected component are adjacent (but the order within a strongly connected
    /// component is undefined).
    /// </summary>
    /// <param name="states">The object states to traverse. The object with array index
    /// 0 is ignored. All other objects are assumed to be reachable from the object
    /// with array index 1.</param>
    internal static int[] ComputeTopologicalOrder(State[] states) {
        Debug.Assert(states.Length > 1);

        // Fill the State.StronglyConnectedComponent fields.
        // (We don't need the returned array, so we can recycle it for our purposes.)
        int[] orderedObjectIndices = FindStronglyConnectedComponents(states);
        Array.Clear(orderedObjectIndices, 0, orderedObjectIndices.Length);
        int nextPosition = orderedObjectIndices.Length - 1;

        TopoIndices = new int[2];
        TopoSubIndices = new int[2];
        SccIndexStack = new int[2];

        // We traverse the graph non-recursively in depth-first order.

        int topoStackCount = 0;
        int sccIndexStackCount = 0;

        int[] visitedBits = new int[(checked(states.Length + 31))/32];

        int objectIndex;
        int[] subObjectIndices;
        {
            var state = states[1];
            if (state.StronglyConnectedComponent == null) {
                objectIndex = 1;
                subObjectIndices = state.ObjectIndices;
                if (subObjectIndices == null) {
                    orderedObjectIndices[1] = 1;
                    return orderedObjectIndices;
                }
                visitedBits[0] = 1 << 1;
            } else {
                foreach (var sccIndex in state.StronglyConnectedComponent)
                    visitedBits[sccIndex/32] |= 1 << (sccIndex%32);
                objectIndex = state.StronglyConnectedComponent[0];
                subObjectIndices = states[objectIndex].ObjectIndices;
                SccIndexStack[0] = 1;
                sccIndexStackCount = 1;
            }
        }
        int subIndex = subObjectIndices.Length - 1;

        for (;;) {
            // First we iterate over the sub objects...
            Debug.Assert(subObjectIndices != null);

            // (The states array was constructed in breadth-first order, while we construct
            //  the topological order using depth-first search. With a bit of luck we can
            //  keep the resulting orderedObjectIndices close to a simple increasing sequence
            //  by iterating over the sub-objects in the depth-first search in reverse order.)
            while (subIndex >= 0) {
                var subObjectIndex = subObjectIndices[subIndex];
                --subIndex;
                if (subObjectIndex == 0) continue;
                int w = subObjectIndex/32, b = subObjectIndex%32;
                if (((visitedBits[w] >> b) & 1) == 0) {
                    var subState = states[subObjectIndex];
                    var subSubObjectIndices = subState.ObjectIndices;
                    if (subState.StronglyConnectedComponent == null) {
                        visitedBits[w] |= 1 << b;
                        if (subSubObjectIndices == null) {
                            orderedObjectIndices[nextPosition] = subObjectIndex;
                            --nextPosition;
                            continue;
                        }
                        subObjectIndices = subSubObjectIndices;
                    } else {
                        foreach (var sccIndex in subState.StronglyConnectedComponent)
                            visitedBits[sccIndex/32] |= 1 << (sccIndex%32);
                        subObjectIndex = subState.StronglyConnectedComponent[0];
                        subObjectIndices = states[subObjectIndex].ObjectIndices;
                        SccIndexStack[sccIndexStackCount] = 1;
                        if (++sccIndexStackCount == SccIndexStack.Length) GrowSccIndexStack();
                    }
                    TopoIndices[topoStackCount] = objectIndex;
                    TopoSubIndices[topoStackCount] = subIndex;
                    ++topoStackCount;
                    if (topoStackCount == TopoIndices.Length) GrowTopoIndices();
                    if (topoStackCount == TopoSubIndices.Length) GrowTopoSubIndices();
                    objectIndex = subObjectIndex;
                    subIndex = subObjectIndices.Length - 1;
                    continue;
                }
            }

            // ... then we iterate over other object in the same strongly connected component.
            var scc = states[objectIndex].StronglyConnectedComponent;
            if (scc == null) {
                orderedObjectIndices[nextPosition] = objectIndex;
                --nextPosition;
            } else {
                Debug.Assert(sccIndexStackCount > 0);
                int sccIndex = SccIndexStack[sccIndexStackCount - 1];
                if (sccIndex < scc.Length) {
                    objectIndex = scc[sccIndex];
                    subObjectIndices = states[objectIndex].ObjectIndices;
                    subIndex = subObjectIndices.Length - 1;
                    SccIndexStack[sccIndexStackCount - 1] = ++sccIndex;
                    continue;
                }
                --sccIndexStackCount;
                for (int i = scc.Length - 1; i >= 0; --i) {
                    sccIndex = scc[i];
                    orderedObjectIndices[nextPosition] = sccIndex;
                    --nextPosition;
                }
            }
            if (topoStackCount == 0) break;
            --topoStackCount;
            objectIndex = TopoIndices[topoStackCount];
            subIndex = TopoSubIndices[topoStackCount];
            subObjectIndices = states[objectIndex].ObjectIndices;
        }
        return orderedObjectIndices;
    }
}

[Flags]
internal enum CloneEvents {
    None = 0,
    OnSerializing = 1,
    OnSerialized = 2,
    OnDeserializing = 4,
    OnDeserialized = 8,
    ISerializable = 16,
    IDeserializationCallback = 32,
    IObjectReference = 64
}

internal sealed class CloneEventHandlers {
    public readonly CloneEvents Events;

    private delegate void Handler(object instance, StreamingContext context);

    private readonly Handler OnSerializingHandler;
    private readonly Handler OnSerializedHandler;
    private readonly Handler OnDeserializingHandler;
    private readonly Handler OnDeserializedHandler;

    private CloneEventHandlers(CloneEvents events,
                               Handler onSerializingHandler,
                               Handler onSerializedHandler,
                               Handler onDeserializingHandler,
                               Handler onDeserializedHandler)
    {
        Events = events;
        OnSerializingHandler = onSerializingHandler;
        OnSerializedHandler = onSerializedHandler;
        OnDeserializingHandler = onDeserializingHandler;
        OnDeserializedHandler = onDeserializedHandler;
    }

    public void InvokeOnSerializing(object instance, StreamingContext context) {
        OnSerializingHandler.Invoke(instance, context);
    }

    public void InvokeOnSerialized(object instance, StreamingContext context) {
        OnSerializedHandler.Invoke(instance, context);
    }

    public void InvokeOnDeserializing(object instance, StreamingContext context) {
        OnDeserializingHandler.Invoke(instance, context);
    }

    public void InvokeOnDeserialized(object instance, StreamingContext context) {
        OnDeserializedHandler.Invoke(instance, context);
    }

    private static readonly CloneEventHandlers ISerializableOnly = new CloneEventHandlers(CloneEvents.ISerializable, null, null, null, null);
    private static readonly CloneEventHandlers ISerializableAndObjectReferenceOnly = new CloneEventHandlers(CloneEvents.ISerializable | CloneEvents.IObjectReference, null, null, null, null);

    private static Handler WithBoxedArgument<T>(Action<T,StreamingContext> handler) {
        return (object obj, StreamingContext context) => handler((T)obj, context);
    }
    private static readonly MethodInfo WithBoxedArgumentMethodInfo = typeof(CloneEventHandlers).GetMethod("WithBoxedArgument", BindingFlags.Static | BindingFlags.NonPublic);

    private static Handler CreateHandler(Type type, MethodInfo mi) {
        var delegateType = typeof(Action<,>).MakeGenericType(type, typeof(StreamingContext));
        var d = Delegate.CreateDelegate(delegateType, null, mi);
        return (Handler)WithBoxedArgumentMethodInfo.MakeGenericMethod(type).Invoke(null, new object[]{d});
    }

    private static readonly Type typeofObject                   = typeof(object);
    private static readonly Type typeofISerializable            = typeof(ISerializable);
    private static readonly Type typeofIObjectReference         = typeof(IObjectReference);
    private static readonly Type typeofIDeserializationCallback = typeof(IDeserializationCallback);
    private static readonly Type typeofOnSerializingAttribute   = typeof(OnSerializingAttribute);
    private static readonly Type typeofOnSerializedAttribute    = typeof(OnSerializedAttribute);
    private static readonly Type typeofOnDeserializingAttribute = typeof(OnDeserializingAttribute);
    private static readonly Type typeofOnDeserializedAttribute  = typeof(OnDeserializedAttribute);

    public static CloneEventHandlers Create(Type type) {
        Debug.Assert(type != null);
        if (type == typeofObject) return null;
        var events = CloneEvents.None;
        if (typeofISerializable.IsAssignableFrom(type)) events |= CloneEvents.ISerializable;
        if (typeofIObjectReference.IsAssignableFrom(type)) events |= CloneEvents.IObjectReference;
        if (typeofIDeserializationCallback.IsAssignableFrom(type)) events |= CloneEvents.IDeserializationCallback;
        var bt = type;
        for (;;) {
            var methods = bt.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.DeclaredOnly);
            for (int i = 0; i < methods.Length; ++i) {
                var mi = methods[i];
                if (   mi.IsDefined(typeofOnSerializingAttribute, false)
                    || mi.IsDefined(typeofOnSerializedAttribute, false)
                    || mi.IsDefined(typeofOnDeserializingAttribute, false)
                    || mi.IsDefined(typeofOnDeserializedAttribute, false)) return CreateContinue(type, events, bt, methods, i);
            }
            bt = bt.BaseType;
            if (bt == null || bt == typeofObject) break;
            if (!bt.IsSerializable) throw new SerializationException(Cloner.BaseTypeNotSerializableMessage(bt, type));
       }
       if (events == 0) return null;
       if (events == CloneEvents.ISerializable) return ISerializableOnly;
       if (events == (CloneEvents.ISerializable | CloneEvents.IObjectReference)) return ISerializableAndObjectReferenceOnly;
       return new CloneEventHandlers(events, null, null, null, null);
    }
    private static CloneEventHandlers CreateContinue(Type type, CloneEvents events, Type baseType, MethodInfo[] methods, int i) {
        Delegate onSerializingHandler = null, onSerializedHandlers = null, onDeserializingHandlers = null, onDeserializedHandlers = null;
        var bt = baseType;
        for (;;) {
            for (; i < methods.Length; ++i) {
                var mi = methods[i];
                if (mi.IsDefined(typeofOnSerializingAttribute, false)) {
                    var d = CreateHandler(bt, mi);
                    onSerializingHandler = onSerializingHandler == null ? d : Delegate.Combine(d, onSerializingHandler); // call base handler first
                }
                if (mi.IsDefined(typeofOnSerializedAttribute, false)) {
                    var d = CreateHandler(bt, mi);
                    onSerializedHandlers = onSerializedHandlers == null ? d : Delegate.Combine(d, onSerializedHandlers);
                }
                if (mi.IsDefined(typeofOnDeserializingAttribute, false)) {
                    var d = CreateHandler(bt, mi);
                    onDeserializingHandlers = onDeserializingHandlers == null ? d : Delegate.Combine(d, onDeserializingHandlers);
                }
                if (mi.IsDefined(typeofOnDeserializedAttribute, false)) {
                    var d = CreateHandler(bt, mi);
                    onDeserializedHandlers = onDeserializedHandlers == null ? d : Delegate.Combine(d, onDeserializedHandlers);
                }
            }
            bt = bt.BaseType;
            if (bt == null || bt == typeofObject) break;
            if (!bt.IsSerializable) throw new SerializationException(Cloner.BaseTypeNotSerializableMessage(bt, type));
            methods = bt.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.DeclaredOnly);
            i = 0;
        }
        Handler onSerializing = null, onSerialized = null, onDeserializing = null, onDeserialized = null;
        if (onSerializingHandler != null) {
            events |= CloneEvents.OnSerializing;
            onSerializing = (Handler)onSerializingHandler;
        }
        if (onSerializedHandlers != null) {
            events |= CloneEvents.OnSerialized;
            onSerialized = (Handler)onSerializedHandlers;
        }
        if (onDeserializingHandlers != null) {
            events |= CloneEvents.OnDeserializing;
            onDeserializing = (Handler)onDeserializingHandlers;
        }
        if (onDeserializedHandlers != null) {
            events |= CloneEvents.OnDeserialized;
            onDeserialized = (Handler)onDeserializedHandlers;
        }
        return new CloneEventHandlers(events, onSerializing, onSerialized, onDeserializing, onDeserialized);
    }
}

}

#endif
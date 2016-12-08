// Copyright (c) Stephan Tolksdorf 2010-2011
// License: Simplified BSD License. See accompanying documentation.

module FParsec.Test.CloningTests

#if LOW_TRUST
#else
open FParsec.Test.Test

open FParsec.Cloning

// The organization of this test module is a bit messy currently,
// so "Go to definition" is your friend here.
// The important point is that the code coverage is close to 100%.

type SerializationInfo = System.Runtime.Serialization.SerializationInfo
type StreamingContext = System.Runtime.Serialization.StreamingContext
type OnSerializingAttribute = System.Runtime.Serialization.OnSerializingAttribute
type OnSerializedAttribute = System.Runtime.Serialization.OnSerializedAttribute
type OnDeserializingAttribute = System.Runtime.Serialization.OnDeserializingAttribute
type OnDeserializedAttribute = System.Runtime.Serialization.OnDeserializedAttribute
type ISerializable   = System.Runtime.Serialization.ISerializable
type IObjectReference = System.Runtime.Serialization.IObjectReference
type IDeserializationCallback = System.Runtime.Serialization.IDeserializationCallback
type SerializationException = System.Runtime.Serialization.SerializationException

type CloneEvents = FParsec.Cloning.CloneEvents
type CloneEventHandlers = FParsec.Cloning.CloneEventHandlers

type BindingFlags = System.Reflection.BindingFlags

type KeyValuePair<'k,'v> = System.Collections.Generic.KeyValuePair<'k,'v>


[<AllowNullLiteral>]
type TestState(objectIndices: int[]) =
    inherit Cloner.State(null, objectIndices)

    override t.Type =
        raise (System.NotImplementedException())

    override t.CreateUninitializedObject() =
        raise (System.NotImplementedException())

    override t.WriteToUninitializedObject(instance, objectGraph) =
        raise (System.NotImplementedException())

// a reference implementation
let findStronglyConnectedComponents (states: Cloner.State[]) =
    let stack = new System.Collections.Generic.Stack<int>()
    let index = ref 1
    let indices  = Array.zeroCreate states.Length
    let lowlinks = Array.zeroCreate states.Length

    let rec tarjan (v: int) =
        indices.[v] <- !index
        lowlinks.[v] <- !index
        incr index
        stack.Push(v)
        let objectIndices = states.[v].ObjectIndices
        if objectIndices <> null then
            for w in objectIndices do
                if w <> 0 then
                    if indices.[w] = 0 then
                        tarjan w
                        lowlinks.[v] <- min lowlinks.[v] lowlinks.[w]
                    else if stack.Contains(w) then
                        lowlinks.[v] <- min lowlinks.[v] indices.[w]
        if lowlinks.[v] = indices.[v] then
            let mutable last = stack.Pop()
            if last <> v then
                let scc = new System.Collections.Generic.List<int>()
                scc.Add(last)
                while last <> v do
                    last <- stack.Pop()
                    scc.Add(last)
                let scc = scc.ToArray()
                for i in scc do
                    states.[i].StronglyConnectedComponent <- scc
    tarjan 1


let copyTestStates (states: TestState[]) =
    states |> Array.map (fun s -> if s <> null then new TestState(s.ObjectIndices) else null)

let upcastTestStates (states: TestState[]) = box states :?> Cloner.State[]

let createRandomTestStateGraph (rand: System.Random) n =
    let states = Array.zeroCreate (n + 1)
    let p = rand.NextDouble()
    let p = p*p
    for i = 2 to n do
        let indices = ResizeArray()
        for j = 1 to n do
            if rand.NextDouble() < p then indices.Add(j)
        if indices.Count = 0 && rand.NextDouble() < 0.5 then
            states.[i] <- new TestState(null)
        else
            if rand.NextDouble() < 0.5 then indices.Add(0) // 0 values should be ignored
            let indices = indices.ToArray()
            shuffleArray rand indices
            states.[i] <- new TestState(indices)

    // find all roots
    let visited = Array.zeroCreate (n + 1)
    let unvisitedIndices = [|2..n|]
    let unvisitedCount = ref (n - 1)
    let roots = ResizeArray()
    while !unvisitedCount <> 0 do
        let oldUnvisitedCount = !unvisitedCount
        let rec mark index =
            visited.[index] <- 1uy
            decr unvisitedCount
            let indices = states.[index].ObjectIndices
            if indices <> null then
                for idx in indices do
                    if idx > 1 && visited.[idx] = 0uy then
                        mark idx
        let index = unvisitedIndices.[rand.Next(oldUnvisitedCount)]
        roots.Add(index)
        mark index
        // remove newly marked indices from unvisitedIndices
        let mutable lag = 0
        for i = 0 to oldUnvisitedCount - 1 do
            if visited.[unvisitedIndices.[i]] <> 0uy then lag <- lag + 1
            elif lag <> 0 then
                unvisitedIndices.[i - lag] <- unvisitedIndices.[i]

    if rand.NextDouble() < p then roots.Add(1)
    states.[1] <- TestState(roots.ToArray())
    states



let testStronglyConnectedComponents() =
    let test (states: TestState[]) =
        states.[0] |> Equal null

        let states2 = upcastTestStates (copyTestStates states)
        let states = upcastTestStates states
        let components = Cloner.FindStronglyConnectedComponents(states)

        components.[0] |> Equal 0
        components.[0] <- System.Int32.MaxValue
        (components |> Array.tryFind (fun c -> c <= 0)).IsNone |> True
        // basic consistency checks
        for i = 1 to states.Length - 1 do
            let c = components.[i]
            if c <> 0 then
                let state = states.[i]
                let scc = state.StronglyConnectedComponent
                if scc = null then
                    components.[i] <- 0
                else
                    for j in scc do
                        states.[j].StronglyConnectedComponent |> ReferenceEqual scc
                        components.[j] |> Equal c
                        components.[j] <- 0
                System.Array.IndexOf(components, c) |> Equal -1
        for i = 1 to states.Length - 1 do
            components.[i] |> Equal 0

        // compare result with reference implementation
        findStronglyConnectedComponents states2
        for i = 1 to states.Length - 1 do
            let state1 = states.[i]
            let state2 = states2.[i]
            let scc1 = state1.StronglyConnectedComponent
            let scc2 = state2.StronglyConnectedComponent
            if scc2 = null then
                scc1 |> IsNull
            elif not (obj.ReferenceEquals(scc1, scc2)) then
                System.Array.Sort(scc1)
                System.Array.Sort(scc2)
                scc1 |> Equal scc2
                // speed up future comparisons
                for j in state2.StronglyConnectedComponent do
                    states2.[j].StronglyConnectedComponent <- scc1

    test [|null; TestState(null)|]
    test [|null; TestState([|1|])|]

    let rand = System.Random(1234)
    for i = 0 to 1000 do
        let graph = createRandomTestStateGraph rand 7
        test graph
    for i = 8 to 300 do
        let graph = createRandomTestStateGraph rand i
        test graph

let testComputeTopologicalOrder() =
    let test (states: Cloner.State[]) =
        let order = Cloner.ComputeTopologicalOrder(states)
        let marked = Array.zeroCreate states.Length

        // check that each index only occurs once
        for index in order do
            marked.[index] |> Equal 0uy
            marked.[index] <- 1uy
        System.Array.IndexOf(marked, 0uy) |> Equal -1
        System.Array.Clear(marked, 0, marked.Length)

        // check dependency order
        marked.[0] <- 1uy // states.[0] is ignored
        for i = order.Length - 1 downto 1 do
            let index = order.[i]
            if marked.[index] = 0uy then
                let state = states.[index]
                if state.StronglyConnectedComponent = null then
                    marked.[index] <- 1uy
                    // all dependencies must be marked
                    if state.ObjectIndices <> null then
                        for j in state.ObjectIndices do
                            marked.[j] |> Equal 1uy
                else
                    // objects within a strongly connected components have no defined order
                    for j in state.StronglyConnectedComponent do
                        marked.[j] <- 1uy
                    for j in state.StronglyConnectedComponent do
                        for k in states.[j].ObjectIndices do
                            marked.[k] |> Equal 1uy


    test [|null; TestState(null)|]
    test [|null; TestState([|1|])|]

    let rand = System.Random(1234)
    for i = 0 to 1000 do
        let graph = createRandomTestStateGraph rand 7
        test (upcastTestStates graph)
    for i = 8 to 300 do
        let graph = createRandomTestStateGraph rand i
        test (upcastTestStates graph)


let callStreamingContextCallback (context: StreamingContext) =

    (context.Context :?> (unit -> unit))()

let addToContextList (context: StreamingContext) s =
    let r = context.Context :?> string list ref
    r:= s::!r

[<AutoSerializable(false)>]
type NonSerializableBase() = class end

type ClassWithNonSerializableBase() =
    inherit NonSerializableBase()

type ClassWithSingleOnSerializingHandler() =
    member t.AnotherMethod() = ()
    [<OnSerializing>]
    member private t.OnSerializing(context) =
        addToContextList context "OnSerializing"

type ClassWithSingleOnSerializingHandlerAndNonSerializableBase() =
    inherit NonSerializableBase()
    member t.AnotherMethod() = ()
    [<OnSerializing>]
    member private t.OnSerializing(context) =
        addToContextList context "OnSerializing"


type ClassWithSingleOnSerializingHandlerInBase() =
    inherit ClassWithSingleOnSerializingHandler()

type ClassWithSingleOnSerializingHandlerInBaseBase() =
    inherit ClassWithSingleOnSerializingHandlerInBase()

type ClassWithSingleOnSerializedHandler() =
    member t.AnotherMethod() = ()
    [<OnSerialized>]
    member private t.OnSerialized(context) =
        addToContextList context "OnSerialized"

type ClassWithSingleOnDeserializingHandler() =
    member t.AnotherMethod() = ()
    [<OnDeserializing>]
    member private t.OnDeserializing(context) =
        addToContextList context "OnDeserializing"

type ClassWithSingleOnDeserializedHandler() =
    member t.AnotherMethod() = ()
    [<OnDeserialized>]
    member private t.OnDeserialized(context) =
        addToContextList context "OnDeserialized"

type ClassThatHasItAllBaseBase() =
    [<OnSerializing>]
    member private t.OnSerializing(context) = addToContextList context "OnSerializingBaseBase"
    [<OnDeserializing>]
    member private t.OnDeserializing(context) = addToContextList context "OnDeserializingBaseBase"

type ClassThatHasItAllBase() =
    inherit ClassThatHasItAllBaseBase()
    [<OnSerializing>]
    member private t.OnSerializing(context) = addToContextList context "OnSerializingBase"
    [<OnDeserializing>]
    member private t.OnDeserializing(context) = addToContextList context "OnDeserializingBase"

type ClassThatHasItAll() =
    inherit ClassThatHasItAllBase()
    member private t.OnSerializin(context) = raise (System.NotImplementedException())
    [<OnSerializing>]
    member private t.OnSerializing(context) = addToContextList context "OnSerializing"
    [<OnSerialized>]
    member private t.OnSerialized(context) = addToContextList context "OnSerialized"
    [<OnDeserializing>]
    member private t.OnDeserializing(context) = addToContextList context "OnDeserializing"
    [<OnDeserialized>]
    member private t.OnDeserialized(context) = addToContextList context "OnDeserialized"


    interface ISerializable with
        member t.GetObjectData(info, context) = raise (System.NotImplementedException())

    interface IDeserializationCallback with
        member t.OnDeserialization(sender) = raise (System.NotImplementedException())

    interface IObjectReference with
        member t.GetRealObject(context) = raise (System.NotImplementedException())


let testCloningEventHandlers() =
    let contextList = ref []
    let context = StreamingContext(System.Runtime.Serialization.StreamingContextStates.Clone, contextList)

    let () =
        CloneEventHandlers.Create(typeof<obj>) |> IsNull
        CloneEventHandlers.Create(typeof<unit>) |> IsNull

    let () =
        contextList:= []
        let instance = ClassWithSingleOnSerializingHandler()
        let handlers = CloneEventHandlers.Create(instance.GetType())
        handlers.Events |> Equal CloneEvents.OnSerializing
        handlers.InvokeOnSerializing(instance, context)
        !contextList |> Equal ["OnSerializing"]

    let () =
        contextList:= []
        let instance = ClassWithSingleOnSerializingHandlerInBase()
        let handlers = CloneEventHandlers.Create(instance.GetType())
        handlers.Events |> Equal CloneEvents.OnSerializing
        handlers.InvokeOnSerializing(instance, context)
        !contextList |> Equal ["OnSerializing"]

    let () =
        contextList:= []
        let instance = ClassWithSingleOnSerializingHandlerInBaseBase()
        let handlers = CloneEventHandlers.Create(instance.GetType())
        handlers.Events |> Equal CloneEvents.OnSerializing
        handlers.InvokeOnSerializing(instance, context)
        !contextList |> Equal ["OnSerializing"]

    let () =
        contextList:= []
        let instance = ClassWithSingleOnSerializedHandler()
        let handlers = CloneEventHandlers.Create(instance.GetType())
        handlers.Events |> Equal CloneEvents.OnSerialized
        handlers.InvokeOnSerialized(instance, context)
        !contextList |> Equal ["OnSerialized"]

    let () =
        contextList:= []
        let instance = ClassWithSingleOnDeserializingHandler()
        let handlers = CloneEventHandlers.Create(instance.GetType())
        handlers.Events |> Equal CloneEvents.OnDeserializing
        handlers.InvokeOnDeserializing(instance, context)
        !contextList |> Equal ["OnDeserializing"]

    let () =
        contextList:= []
        let instance = ClassWithSingleOnDeserializedHandler()
        let handlers = CloneEventHandlers.Create(instance.GetType())
        handlers.Events |> Equal CloneEvents.OnDeserialized
        handlers.InvokeOnDeserialized(instance, context)
        !contextList |> Equal ["OnDeserialized"]

    let () =
        let instance = {new ISerializable with
                           member t.GetObjectData(info, context) = raise (System.NotImplementedException())}
        let handlers = CloneEventHandlers.Create(instance.GetType())
        handlers.Events |> Equal CloneEvents.ISerializable

    let () =
        let instance = {new IDeserializationCallback with
                            member t.OnDeserialization(sender) = raise (System.NotImplementedException())}
        let handlers = CloneEventHandlers.Create(instance.GetType())
        handlers.Events |> Equal CloneEvents.IDeserializationCallback

    let () =
        let instance = {new ISerializable with
                           member t.GetObjectData(info, context) = raise (System.NotImplementedException())
                        interface IObjectReference with
                            member t.GetRealObject(context) = raise (System.NotImplementedException())}
        let handlers = CloneEventHandlers.Create(instance.GetType())
        handlers.Events |> Equal (CloneEvents.ISerializable ||| CloneEvents.IObjectReference)

    let () =
        let instance = {new IObjectReference with
                            member t.GetRealObject(context) = raise (System.NotImplementedException())}
        let handlers = CloneEventHandlers.Create(instance.GetType())
        handlers.Events |> Equal CloneEvents.IObjectReference

    let () =
        let instance = new ClassThatHasItAll()
        let handlers = CloneEventHandlers.Create(instance.GetType())
        handlers.Events |> Equal (    CloneEvents.OnSerializing
                                  ||| CloneEvents.OnSerialized
                                  ||| CloneEvents.OnDeserializing
                                  ||| CloneEvents.OnDeserialized
                                  ||| CloneEvents.ISerializable
                                  ||| CloneEvents.IDeserializationCallback
                                  ||| CloneEvents.IObjectReference)
        contextList:= []
        handlers.InvokeOnSerializing(instance, context)
        !contextList |> Equal ["OnSerializing"; "OnSerializingBase"; "OnSerializingBaseBase"]
        contextList:= []
        handlers.InvokeOnSerialized(instance, context)
        !contextList |> Equal ["OnSerialized"]
        contextList:= []
        handlers.InvokeOnDeserializing(instance, context)
        !contextList |> Equal ["OnDeserializing"; "OnDeserializingBase"; "OnDeserializingBaseBase"]
        contextList:= []
        handlers.InvokeOnDeserialized(instance, context)
        !contextList |> Equal ["OnDeserialized"]

    try
        CloneEventHandlers.Create(typeof<ClassWithNonSerializableBase>) |> ignore
        Fail()
    with :? SerializationException -> ()

    try
        CloneEventHandlers.Create(typeof<ClassWithSingleOnSerializingHandlerAndNonSerializableBase>) |> ignore
        Fail()
    with :? SerializationException -> ()

type TypeWithNoSerializedField() =
    [<DefaultValue; System.NonSerialized>]
    val mutable Value: int
    [<DefaultValue; System.NonSerialized>]
    val mutable OnSerializedWasCalled: bool
    [<DefaultValue; System.NonSerialized>]
    val mutable OnDeserializedWasCalled: bool

    [<OnSerialized>]
    member private t.OnSerialized(context: StreamingContext) =
        t.OnSerializedWasCalled |> False
        t.OnSerializedWasCalled <- true

    [<OnDeserialized>]
    member private t.OnDeserialized(context: StreamingContext) =
        t.OnDeserializedWasCalled |> False
        t.OnDeserializedWasCalled <- true

type BlittableType(val1: int, val2: string, val3: KeyValuePair<int,string>) =
    member t.Value1 = val1
    member t.Value2 = val2
    member t.Value3 = val3

[<Struct>]
type BlittableStructType(val1: int, val2: string, val3: KeyValuePair<int,string>) =
    member t.Value1 = val1
    member t.Value2 = val2
    member t.Value3 = val3

type NonBlittableType1 =
    val Value1: int
    val Value2: int[]
    val Value3: KeyValuePair<int,string>

type NonBlittableType2 =
    val Value1: int
    [<System.NonSerialized>]
    val Value2: string
    val Value3: KeyValuePair<int,string>

type NonBlittableType3 =
    val Value1: int
    val Value2: string
    val Value3: KeyValuePair<int,int[]>

type BlittableTypeWithBase =
    inherit BlittableType
    val Value1: int
    val Value2: string
    val Value3: KeyValuePair<int,string>

type BlittableTypeWithNonBlittableBase1 =
    inherit NonBlittableType1
    val Value1: int

type BlittableTypeWithNonBlittableBase2 =
    inherit NonBlittableType2
    val Value1: int

type BlittableTypeWithNonBlittableBase3 =
    inherit NonBlittableType3
    val Value1: int
    [<System.NonSerialized>]
    val Value2: int

type BlittableTypeWithNonBlittableBase4 =
    inherit BlittableTypeWithNonBlittableBase3
    val Value1: int

let getFieldValues (fields: System.Reflection.FieldInfo[]) (instance: obj) =
    let values = Array.zeroCreate fields.Length
    for i = 0 to fields.Length - 1 do
        let f = fields.[i]
        values.[i] <- f.GetValue(instance)
    values

let testGetSerializedFields() =
    let mutable blittable = false
    Cloner.GetSerializedFields(typeof<obj>, &blittable).Length |> Equal 0
    blittable |> True
    Cloner.GetSerializedFields(typeof<unit>, &blittable).Length |> Equal 0
    blittable |> True
    let test (ty: System.Type) =
        let rec getFields (ty: System.Type) =
            let fields =
                ty.GetFields(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)
                |> Array.filter (fun field -> not field.IsNotSerialized)
            let baseType = ty.BaseType
            if baseType = null then fields
            else Array.append fields (getFields baseType)

        let rec isBlittableField (field: System.Reflection.FieldInfo) =
            let ty = field.FieldType
            ty.IsPrimitive || ty = typeof<string> || (ty.IsValueType && getFields ty |> Array.forall isBlittableField)

        let mutable isBlittable = false
        let fields = Cloner.GetSerializedFields(ty, &isBlittable)
        let fields2 = getFields ty
        let isBlittable2 = fields2 |> Array.forall isBlittableField
        fields.Length |> Equal fields2.Length
        for i = 0 to fields2.Length - 1 do
            let f1, f2 = fields.[i], fields2.[i]
            f1.Name |> Equal f2.Name
            f1.FieldType |> Equal f2.FieldType
            f1.DeclaringType |> Equal f2.DeclaringType
        isBlittable |> Equal isBlittable

    test typeof<TypeWithNoSerializedField>
    test typeof<BlittableType>
    test typeof<BlittableStructType>
    test typeof<NonBlittableType1>
    test typeof<NonBlittableType2>
    test typeof<NonBlittableType3>
    test typeof<BlittableTypeWithBase>
    test typeof<BlittableTypeWithNonBlittableBase1>
    test typeof<BlittableTypeWithNonBlittableBase2>
    test typeof<BlittableTypeWithNonBlittableBase3>
    test typeof<BlittableTypeWithNonBlittableBase4>

    try
        test typeof<ClassWithNonSerializableBase>
        Fail()
    with :? SerializationException -> ()

let testCreateFieldValuesGetter() =
    let test (instance: obj) =
        let mutable isBlittable = false
        let fields = Cloner.GetSerializedFields(instance.GetType(), &isBlittable)
        let fieldValuesGetter = Cloner.CreateFieldValuesGetter(instance.GetType(), fields)
        let values = getFieldValues fields instance
        fieldValuesGetter.Invoke(instance) |> Equal values

    test (BlittableType(1, "2", KeyValuePair(3, "4")))
    test (BlittableStructType(1, "2", KeyValuePair(3, "4")))

let invokeSetter (setter: System.Action<obj, obj[], int[], obj[]>) (instance: obj) (values: obj[]) (objectIndices: int[]) (objectGraph: obj[]) =
    setter.Invoke(instance, values, objectIndices, objectGraph)

let testCreateFieldValuesSetter() =
    let test (setter: System.Action<obj, obj[], int[], obj[]>) (instance: obj) (values: obj[]) (objectIndices: int[]) (objectGraph: obj[]) result =
        invokeSetter setter instance values objectIndices objectGraph
        instance |> Equal result

    let r1 = (1, "2", KeyValuePair(3, "4"))
    let ty1 = r1.GetType()
    let fields1 = ty1.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance)
    let setter1 = Cloner.CreateFieldValuesSetter(ty1, fields1)
    test setter1 (-1, "-1", KeyValuePair(-1, "-1")) [|1; "2"; null|] [|0; 0; 1|] [|null; KeyValuePair(3, "4")|] r1
    test setter1 (-1, "-1", KeyValuePair(-1, "-1")) [|1; "2"; KeyValuePair(3, "4")|] [|0; 0; 0|] [||] r1
    test setter1 (-1, "-1", KeyValuePair(-1, "-1")) [|null; null; null|] [|3; 2; 1|] [|null; KeyValuePair(3, "4"); "2"; 1;|] r1

    let r2 = (BlittableStructType(1, "2", KeyValuePair(3, "4")))
    let ty2 = r2.GetType()
    let fields2 = ty2.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance)
    let setter2 = Cloner.CreateFieldValuesSetter(ty2, fields2)
    test setter2 (BlittableStructType(-1, "-1", KeyValuePair(-1, "-1"))) [|1; "2"; null|] [|0; 0; 1|] [|null; KeyValuePair(3, "4")|] r2
    test setter2 (BlittableStructType(-1, "-1", KeyValuePair(-1, "-1"))) [|1; "2"; KeyValuePair(3, "4")|] [|0; 0; 0|] [||] r2
    test setter2 (BlittableStructType(-1, "-1", KeyValuePair(-1, "-1"))) [|null; null; null|] [|3; 2; 1|] [|null; KeyValuePair(3, "4"); "2"; 1;|] r2


type SerializableConstructorTestClass(calledFromISerializableConstructor: bool) =
    member t.CalledFromISerializableConstructor = calledFromISerializableConstructor

    private new (info: SerializationInfo, context: StreamingContext) =
        info.GetBoolean("ok") |> True
        new SerializableConstructorTestClass(true)


type SerializableConstructorTestStruct = struct
    val mutable CalledFromISerializableConstructor: bool

    private new (info: SerializationInfo, context: StreamingContext) =
        info.GetBoolean("ok") |> True
        {CalledFromISerializableConstructor = true}
end

let testCreateISerializableConstructorCaller() =
    let context = new StreamingContext(System.Runtime.Serialization.StreamingContextStates.Clone)
    let info = new SerializationInfo(typeof<SerializableConstructorTestClass>, new System.Runtime.Serialization.FormatterConverter())
    info.AddValue("ok", true)

    let instance1 = SerializableConstructorTestClass(false)
    instance1.CalledFromISerializableConstructor |> False
    let constructor1 = instance1.GetType().GetConstructor(BindingFlags.NonPublic ||| BindingFlags.Instance, null, [|typeof<SerializationInfo>; typeof<StreamingContext>|], null)
    let constructorCaller1 = Cloner.CreateISerializableConstructorCaller(constructor1)
    constructorCaller1.Invoke(instance1, info, context)
    instance1.CalledFromISerializableConstructor |> True

    let mutable instance2 = SerializableConstructorTestStruct()
    instance2.CalledFromISerializableConstructor |> False
    let constructor2 = instance2.GetType().GetConstructor(BindingFlags.NonPublic ||| BindingFlags.Instance, null, [|typeof<SerializationInfo>; typeof<StreamingContext>|], null)
    let constructorCaller2 = Cloner.CreateISerializableConstructorCaller(constructor2)
    let boxedInstance2 = box instance2
    constructorCaller2.Invoke(boxedInstance2, info, context)
    instance2 <- unbox boxedInstance2
    instance2.CalledFromISerializableConstructor |> True


type NativeSerializationTestType(val1: int, val2: string, val3: obj, val4: obj[]) =
    member t.Value1 = val1
    member t.Value2 = val2
    member t.Value3 = val3
    member t.Value4 = val4

let equalityCacheComparer =
    {new System.Collections.Generic.EqualityComparer<obj*obj>() with
         override t.Equals((x1, x2), (y1, y2)) =
            obj.ReferenceEquals(x1, y1) && obj.ReferenceEquals(x2, y2)

         override t.GetHashCode((x1, x2)) =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(x1)}

let equalityCache = System.Collections.Generic.Dictionary<obj*obj, bool>(equalityCacheComparer)

/// can deal with recursive values, i.e. cyclic object graphs
let recEquals (value1: obj) (value2: obj) =
    let vv = (value1, value2)
    let mutable b = false
    if equalityCache.TryGetValue(vv, &b) then b
    else
        equalityCache.Add(vv, true)
        b <- value1 = value2
        equalityCache.[vv] <- b
        b

let mutable onDeserializedList = [] : int list

[<AllowNullLiteral>]
type NativeSerializationTestClassWithUnorderedEvents<'t>(id: int, value: 't) =
    let mutable value = value
    member t.Id = id
    member t.Value with get() = value
                    and set v = value <- v

    [<DefaultValue; System.NonSerialized>]
    val mutable OnSerializingWasCalled: bool
    [<DefaultValue; System.NonSerialized>]
    val mutable OnSerializedWasCalled: bool
    [<DefaultValue; System.NonSerialized>]
    val mutable OnDeserializingWasCalled: bool
    [<DefaultValue; System.NonSerialized>]
    val mutable DeserializationCallbackWasCalled: bool

    [<OnSerializing>]
    member private t.OnSerializing(context: StreamingContext) =
        t.OnSerializingWasCalled |> False
        t.OnSerializingWasCalled <- true

    [<OnSerialized>]
    member private t.OnSerialized(context: StreamingContext) =
        t.OnSerializingWasCalled |> True
        t.OnSerializedWasCalled  |> False
        t.OnSerializedWasCalled <- true

    [<OnDeserializing>]
    member private t.OnDeserializing(context: StreamingContext) =
        t.OnSerializedWasCalled    |> False
        t.OnDeserializingWasCalled |> False
        t.OnDeserializingWasCalled <- true

    interface IDeserializationCallback with
        member t.OnDeserialization(sender) =
            t.OnSerializingWasCalled |> False
            t.OnDeserializingWasCalled |> True
            t.DeserializationCallbackWasCalled |> False
            t.DeserializationCallbackWasCalled <- true

    override t.Equals(obj) =
        match obj with
        | :? NativeSerializationTestClassWithUnorderedEvents<'t> as o ->
            id = o.Id && recEquals value o.Value
        | _ -> false

    override t.GetHashCode() = raise (System.NotImplementedException())

[<AllowNullLiteral>]
type NativeSerializationTestClass<'t>(id_, value_) =
    inherit NativeSerializationTestClassWithUnorderedEvents<'t>(id_, value_)

    [<DefaultValue; System.NonSerialized>]
    val mutable OnDeserializedWasCalled: bool
    [<DefaultValue; System.NonSerialized>]
    val mutable DeserializationCallbackWasCalled: bool

    [<OnDeserialized>]
    member private t.OnDeserialized(context: StreamingContext) =
        t.OnSerializingWasCalled |> False
        t.OnDeserializingWasCalled |> True
        t.OnDeserializedWasCalled |> False
        t.OnDeserializedWasCalled <- true
        onDeserializedList <- t.Id::onDeserializedList

    interface IDeserializationCallback with
        member t.OnDeserialization(sender) =
            t.OnSerializingWasCalled |> False
            t.OnDeserializingWasCalled |> True
            t.OnDeserializedWasCalled |> True
            t.DeserializationCallbackWasCalled |> False
            t.DeserializationCallbackWasCalled <- true

    override t.Equals(o) =
        match o with
        | :? NativeSerializationTestClass<'t> as o ->
            t.Id = o.Id && recEquals t.Value o.Value
        | _ -> false

    override t.GetHashCode() = raise (System.NotImplementedException())

[<AllowNullLiteral>]
type NativeSerializationTestClass2<'t, 't2>(id_, value_, value2: 't2) =
    inherit NativeSerializationTestClass<'t>(id_, value_)

    member t.Value2 = value2

    override t.Equals(o) =
        match o with
        | :? NativeSerializationTestClass2<'t,'t2> as o ->
            t.Id = o.Id && recEquals t.Value o.Value && recEquals t.Value2 o.Value2
        | _ -> false

    override t.GetHashCode() = raise (System.NotImplementedException())

[<AllowNullLiteral>]
type CustomSerializationTestClass<'t> private (id: int, value: 't, isConstructedFromSerializationInfo: bool) =
    let mutable value = value

    member t.Id = id
    member t.Value with get() = value
                    and set v = value <- v

    [<DefaultValue>]
    static val mutable private GetObjectDataCounter: int

    interface ISerializable with
        member t.GetObjectData(info, context) =
            let c = CustomSerializationTestClass<'t>.GetObjectDataCounter
            CustomSerializationTestClass<'t>.GetObjectDataCounter <- c + 1
            if c%3 <> 0 then
                info.AddValue("id", id)
                info.AddValue("value", value)
            else
                info.AddValue("value", value)
                info.AddValue("id", id)

    public new (id, value) = CustomSerializationTestClass(id, value, false)

    private new (info: SerializationInfo, context: StreamingContext) =
        CustomSerializationTestClass(info.GetValue("id", typeof<int>) :?> int,
                                     info.GetValue("value", typeof<'t>) :?> 't,
                                     true)

    member t.IsConstructedFromSerializationInfo = isConstructedFromSerializationInfo

    [<DefaultValue; System.NonSerialized>]
    val mutable OnSerializingWasCalled: bool
    [<DefaultValue; System.NonSerialized>]
    val mutable OnSerializedWasCalled: bool
    [<DefaultValue; System.NonSerialized>]
    val mutable OnDeserializingWasCalled: bool
    [<DefaultValue; System.NonSerialized>]
    val mutable OnDeserializedWasCalled: bool
    [<DefaultValue; System.NonSerialized>]
    val mutable DeserializationCallbackWasCalled: bool

    [<OnSerializing>]
    member private t.OnSerializing(context: StreamingContext) =
        t.OnSerializingWasCalled |> False
        t.OnSerializingWasCalled <- true

    [<OnSerialized>]
    member private t.OnSerialized(context: StreamingContext) =
        t.OnSerializingWasCalled |> True
        t.OnSerializedWasCalled  |> False
        t.OnSerializedWasCalled <- true

    [<OnDeserializing>]
    member private t.OnDeserializing(context: StreamingContext) =
        t.OnSerializingWasCalled   |> False
        t.OnDeserializingWasCalled |> False
        t.OnDeserializingWasCalled <- true

    [<OnDeserialized>]
    member private t.OnDeserialized(context: StreamingContext) =
        t.OnSerializingWasCalled   |> False
        t.OnDeserializingWasCalled |> True
        t.OnDeserializedWasCalled  |> False
        t.OnDeserializedWasCalled <- true
        onDeserializedList <- id::onDeserializedList

    interface IDeserializationCallback with
        member t.OnDeserialization(sender) =
            t.OnSerializingWasCalled  |> False
            t.OnDeserializedWasCalled |> True
            t.DeserializationCallbackWasCalled |> False
            t.DeserializationCallbackWasCalled <- true

    override t.Equals(o) =
        match o with
        | :? CustomSerializationTestClass<'t> as o ->
            id = o.Id && recEquals value o.Value
        | _ -> false

    override t.GetHashCode() = raise (System.NotImplementedException())

type CustomSerializationTestClassProxyProxy<'t>(id: int, value: 't) =
    interface IObjectReference with
        member t.GetRealObject(context) =
            box (CustomSerializationTestClassWithProxy(id, value))

and CustomSerializationTestClassProxy<'t>(id: int, value: 't) =
    private new (info: SerializationInfo, context: StreamingContext) =
        CustomSerializationTestClassProxy(info.GetValue("id", typeof<int>) :?> int,
                                          info.GetValue("value", typeof<'t>) :?> 't)

    [<DefaultValue; System.NonSerialized>]
    val mutable OnDeserializingWasCalled: bool
    [<DefaultValue; System.NonSerialized>]
    val mutable DeserializationCallbackWasCalled: bool

    [<OnDeserializing>]
    member private t.OnDeserializing(context: StreamingContext) =
        t.OnDeserializingWasCalled |> False
        t.OnDeserializingWasCalled <- true

    interface IDeserializationCallback with
        member t.OnDeserialization(sender) =
            t.OnDeserializingWasCalled |> True
            t.DeserializationCallbackWasCalled |> False
            t.DeserializationCallbackWasCalled <- true

    interface ISerializable with
        member t.GetObjectData(info, context) =
            raise (System.NotImplementedException())

    interface IObjectReference with
        member t.GetRealObject(context) =
            t.OnDeserializingWasCalled |> True
            t.DeserializationCallbackWasCalled |> False
            box (CustomSerializationTestClassProxyProxy(id, value))

and CustomSerializationTestClassProxyWithOnDeserialized<'t>(id: int, value: 't) =
    [<DefaultValue; System.NonSerialized>]
    val mutable OnDeserializingWasCalled: bool
    [<DefaultValue; System.NonSerialized>]
    val mutable OnDeserializedWasCalled: bool
    [<DefaultValue; System.NonSerialized>]
    val mutable DeserializationCallbackWasCalled: bool

    [<OnDeserializing>]
    member private t.OnDeserializing(context: StreamingContext) =
        t.OnDeserializingWasCalled |> False
        t.OnDeserializingWasCalled <- true

    [<OnDeserialized>]
    member private t.OnDeserialized(context: StreamingContext) =
        t.OnDeserializingWasCalled |> True
        t.OnDeserializedWasCalled |> False
        t.OnDeserializedWasCalled <- true

    interface IDeserializationCallback with
        member t.OnDeserialization(sender) =
            t.OnDeserializedWasCalled |> True
            t.DeserializationCallbackWasCalled |> False
            t.DeserializationCallbackWasCalled <- true

    interface IObjectReference with
        member t.GetRealObject(context) =
            t.OnDeserializedWasCalled |> True
            t.DeserializationCallbackWasCalled |> False
            box (CustomSerializationTestClassProxyProxy(id, value))

and CustomSerializationTestClassProxy2<'t> = struct
    val mutable id: int
    val mutable value: 't

    member t.Id    with get() = t.id and set v = t.id <- v
    member t.Value with get() = t.value and set v = t.value <- v

    interface IObjectReference with
        member t.GetRealObject(context) =
            box (CustomSerializationTestClassProxyProxy(t.id, t.value))
end

and CustomSerializationTestClassWithProxy12<'t>(id_, value_) =
    inherit CustomSerializationTestClass<'t>(id_, value_)

    [<DefaultValue>]
    static val mutable private ProxyCounter: int

    interface ISerializable with
        override t.GetObjectData(info, context) =
            info.AddValue("id", t.Id)
            info.AddValue("value", t.Value)
            info.AddValue("unused", "data")
            let c = CustomSerializationTestClassWithProxy12<'t>.ProxyCounter
            CustomSerializationTestClassWithProxy12<'t>.ProxyCounter <- c + 1
            if c%3 <> 0 then
                info.FullTypeName <- typeof<CustomSerializationTestClassProxy<'t>>.FullName
                info.AssemblyName <- typeof<CustomSerializationTestClassProxy<'t>>.Assembly.FullName
            else
                info.FullTypeName <- typeof<CustomSerializationTestClassProxy2<'t>>.FullName
                info.AssemblyName <- typeof<CustomSerializationTestClassProxy2<'t>>.Assembly.FullName

and CustomSerializationTestClassWithProxy<'t>(id_, value_) =
    inherit CustomSerializationTestClass<'t>(id_, value_)

    [<DefaultValue>]
    static val mutable private ProxyCounter: int

    interface ISerializable with
        override t.GetObjectData(info, context) =
            info.AddValue("id", t.Id)
            info.AddValue("value", t.Value)
            info.AddValue("unused", "data")
            let c = CustomSerializationTestClassWithProxy<'t>.ProxyCounter
            CustomSerializationTestClassWithProxy<'t>.ProxyCounter <- c + 1
            info.FullTypeName <- typeof<CustomSerializationTestClassProxy<'t>>.FullName
            info.AssemblyName <- typeof<CustomSerializationTestClassProxy<'t>>.Assembly.FullName


type CustomSerializationTestClassWithSimpleProxyBase() = class end
type CustomSerializationTestClassWithSimpleProxy() =
    interface ISerializable with
        override t.GetObjectData(info, context) =
            info.FullTypeName <- typeof<CustomSerializationTestClassWithSimpleProxyBase>.FullName
            info.AssemblyName <- typeof<CustomSerializationTestClassWithSimpleProxyBase>.Assembly.FullName


type CustomSerializationTestClassWithInvalidProxy<'t>(id_: int, value_: 't) =
    inherit CustomSerializationTestClass<'t>(id_, value_)
    // misses a deserialization constructor
    interface ISerializable with
        override t.GetObjectData(info, context) =
            info.FullTypeName <- typeof<CustomSerializationTestClassWithInvalidProxy<'t>>.FullName
            info.AssemblyName <- typeof<CustomSerializationTestClassWithInvalidProxy<'t>>.Assembly.FullName

type CustomSerializationTestClassInvalidProxy() =
    interface ISerializable with
        member t.GetObjectData(info, context) =
            raise (System.NotImplementedException())

type CustomSerializationTestClassWithInvalidProxy2<'t>(id: int, value: 't) =
    interface ISerializable with
        override t.GetObjectData(info, context) =
            info.FullTypeName <- typeof<CustomSerializationTestClassInvalidProxy>.FullName
            info.AssemblyName <- typeof<CustomSerializationTestClassInvalidProxy>.Assembly.FullName

type CustomSerializationTestClassWithInvalidProxy3<'t>(id: int, value: 't) =
    interface ISerializable with
        override t.GetObjectData(info, context) =
            info.FullTypeName <- typeof<int[]>.FullName
            info.AssemblyName <- typeof<int[]>.Assembly.FullName


type ClassWithNonExistentSerializationProxyClass() =
    interface ISerializable with
        override t.GetObjectData(info, context) =
            info.FullTypeName <- "_NonExistentType"
            info.AssemblyName <- "_NonExistentAssembly"

type ClassWithBuggyObjectReferenceImplementation1() =
    interface IObjectReference with
        member t.GetRealObject(context) =
            null

type ClassWithBuggyObjectReferenceImplementation2() =
    interface IObjectReference with
        member t.GetRealObject(context) =
            box (ClassWithBuggyObjectReferenceImplementation2())

type ClassWithObjectReferenceImplementationThatReturnsThis() =
    interface IObjectReference with
        member t.GetRealObject(context) =
            box t

type ClassWithObjectReferenceImplementationThatReturnsThis2() =
    interface IObjectReference with
        member t.GetRealObject(context) =
            box (ClassWithObjectReferenceImplementationThatReturnsThis())


let testCloners() =
    let testBlittableCloner() =
        let v1 = (BlittableType(1, "2", KeyValuePair(3, "4")))
        let v2 = Cloner.Create(v1.GetType()).Clone(v1) :?> BlittableType
        obj.ReferenceEquals(v1, v2) |> False
        v1.Value1 |> Equal v2.Value1
        v1.Value2 |> Equal v2.Value2
        v1.Value3 |> Equal v2.Value3
        let v3 = Cloner.Create(v1.GetType()).CaptureImage(v1).CreateClone() :?> BlittableType
        obj.ReferenceEquals(v1, v3) |> False
        v1.Value1 |> Equal v3.Value1
        v1.Value2 |> Equal v3.Value2
        v1.Value3 |> Equal v3.Value3

        let v1 = (BlittableStructType(1, "2", KeyValuePair(3, "4")))
        let v2 = Cloner.Create(v1.GetType()).Clone(v1) :?> BlittableStructType
        v1.Value1 |> Equal v2.Value1
        v1.Value2 |> Equal v2.Value2
        v1.Value3 |> Equal v2.Value3
        let v3 = Cloner.Create(v1.GetType()).CaptureImage(v1).CreateClone() :?> BlittableStructType
        v1.Value1 |> Equal v3.Value1
        v1.Value2 |> Equal v3.Value2
        v1.Value3 |> Equal v3.Value3

    testBlittableCloner()

    let testArrayCloners() =
        let EqualArray (a: System.Array) (b: System.Array) =
            let r = b.Rank
            r |> Equal a.Rank
            if r = 1 then // the F# equality comparison is bugged for rank-1 arrays with non-zero lower bound
               let off = a.GetLowerBound(0)
               (b.GetLowerBound(0)) |> Equal off
               for i = 0 to a.GetLength(0) - 1 do
                  b.GetValue(off + i) |> Equal (a.GetValue(off + i))
            else
                a |> Equal b

        let v1 =  box ([||] : int[])
        Cloner.Create(v1.GetType()).Clone(v1) |> Equal v1
        let v2 =  box [|0; 1; 2|]
        Cloner.Create(v1.GetType()).Clone(v2) |> Equal v2
        let v2b = System.Array.CreateInstance(typeof<int>, [|3|], [|1|])
        v2b.SetValue(1, 1)
        v2b.SetValue(2, 2)
        v2b.SetValue(3, 3)
        Cloner.Create(v2b.GetType()).Clone(v2b) :?> System.Array |> EqualArray v2b
        let v3 =  box [|null; "1"; "2"|]
        Cloner.Create(v3.GetType()).Clone(v3) |> Equal v3
        let v4 =  box [|KeyValuePair(1,2); KeyValuePair(3,4)|]
        Cloner.Create(v4.GetType()).Clone(v4) |> Equal v4
        let v5 =  box ([||] : option<int>[])
        Cloner.Create(v5.GetType()).Clone(v5) |> Equal v5
        let v6 =  box [|None; Some 1; Some 2|]
        Cloner.Create(v6.GetType()).Clone(v6) |> Equal v6
        let v6b =  System.Array.CreateInstance(typeof<option<int>>, [|3|], [|1|])
        v6b.SetValue(None, 1)
        v6b.SetValue(Some 2, 2)
        v6b.SetValue(Some 3, 3)
        Cloner.Create(v6b.GetType()).Clone(v6b) :?> System.Array |> EqualArray v6b
        let v7 = [|box (Some 1); null; box [|Some 2; Some 3|]|]
        let cloner = Cloner.Create(v7.GetType())
        cloner.Clone(v7) |> Equal (box v7)
        v7.[0] <- box [|Some 4; Some 5|]
        v7.[1] <- box (Some 6)
        v7.[2] <- null
        cloner.Clone(v7) |> Equal (box v7)

        let v8 = Array3D.zeroCreate<obj> 0 0 0
        Cloner.Create(v8.GetType()).Clone(v8) |> Equal (box v8)

        let v9 = Array3D.zeroCreate 3 4 5
        for i = 0 to 2 do
            for j = 0 to 3 do
                for k = 0 to 4 do
                    v9.[i,j,k] <- i*3*4 + j*5 + k
        Cloner.Create(v9.GetType()).Clone(v9) |> Equal (box v9)

        let v10 = System.Array.CreateInstance(typeof<obj>, [|3; 5; 7|], [|1; 2; 3|])
        for i = 0 to 2 do
            for j = 0 to 4 do
                for k = 0 to 6 do
                    let c = i*5*7 + j*7 + k
                    v10.SetValue((if c%3 = 0 then box [|Some c|]
                                  elif c%5 = 0 then null
                                  else box (Some c)), [|1 + i; 2 + j; 3 + k|])
        Cloner.Create(v10.GetType()).Clone(v10) |> Equal (box v10)

    testArrayCloners()

    let testNativeSerializationCloner() =
        let () =
            let v = TypeWithNoSerializedField()
            let v2 = Cloner.Create(v.GetType()).Clone(v) :?> TypeWithNoSerializedField
            v2.GetType() |> Equal (v.GetType())
            obj.ReferenceEquals(v, v2) |> False
            v.OnSerializedWasCalled |> True
            v.OnDeserializedWasCalled |> False
            v2.OnSerializedWasCalled |> False
            v2.OnDeserializedWasCalled |> True

        let () =
            let v = NativeSerializationTestClassWithUnorderedEvents(1, (2, "3"))
            let v2 = Cloner.Create(v.GetType()).Clone(v) :?> _
            v2 |> Equal v

            v.OnSerializedWasCalled     |> True
            v.OnSerializingWasCalled    |> True
            v.OnDeserializingWasCalled  |> False

            v2.OnSerializingWasCalled   |> False
            v2.OnSerializedWasCalled    |> False
            v2.DeserializationCallbackWasCalled |> True

        let () =
            let v = NativeSerializationTestClass2(1, box "2", (3, "4"))
            let cloner = Cloner.Create(v.GetType())
            let v2 = cloner.Clone(v) :?> _
            v2 |> Equal v

            v.OnSerializedWasCalled    |> True
            v.OnSerializingWasCalled   |> True
            v.OnDeserializingWasCalled |> False
            v.OnDeserializedWasCalled  |> False
            v.DeserializationCallbackWasCalled |> False

            v2.OnSerializingWasCalled  |> False
            v2.OnDeserializedWasCalled |> True
            v2.DeserializationCallbackWasCalled |> True

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            let v3 = cloner.Clone(v) :?> _
            v3 |> Equal v

            v.Value <- Some 2
            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            let v4 = cloner.Clone(v) :?> _
            v4 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            let v5 = cloner.Clone(v) :?> _
            v5 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            v.Value <- ref 2
            let v6 = cloner.Clone(v) :?> _
            v6 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            v.Value <- null
            let v7 = cloner.Clone(v) :?> _
            v7 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            v.Value <- ref 2
            let v8 = cloner.Clone(v) :?> _
            v8 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            v.Value <- "2"
            let v9 = cloner.Clone(v) :?> _
            v9 |> Equal v
        ()
    testNativeSerializationCloner()

    let testCustomSerializationCloner() =
        let () =
            let v = CustomSerializationTestClass(1, box "2")
            let cloner = Cloner.Create(v.GetType())
            let v2 = cloner.Clone(v) :?> _
            v2 |> Equal v

            v.OnSerializedWasCalled    |> True
            v.OnSerializingWasCalled   |> True
            v.OnDeserializingWasCalled |> False
            v.OnDeserializedWasCalled  |> False
            v.DeserializationCallbackWasCalled |> False

            v2.OnSerializingWasCalled  |> False
            v2.OnDeserializedWasCalled |> True
            v2.DeserializationCallbackWasCalled |> True

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            let v3 = cloner.Clone(v) :?> _
            v3 |> Equal v

            v.Value <- Some 2
            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            let v4 = cloner.Clone(v) :?> _
            v4 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            let v5 = cloner.Clone(v) :?> _
            v5 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            v.Value <- ref 2
            let v6 = cloner.Clone(v) :?> _
            v6 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            v.Value <- null
            let v7 = cloner.Clone(v) :?> _
            v7 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            v.Value <- ref 2
            let v8 = cloner.Clone(v) :?> _
            v8 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            v.Value <- "2"
            let v9 = cloner.Clone(v) :?> _
            v9 |> Equal v

        let () =
            let v = box (CustomSerializationTestClassWithProxy12(1, KeyValuePair("2", 3))) :?> CustomSerializationTestClass<KeyValuePair<string,int>>
            let cloner = Cloner.Create(v.GetType())
            let v2 = cloner.Clone(v) :?> _
            v2 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            let v3 = cloner.Clone(v) :?> _
            v3 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            let v4 = cloner.Clone(v) :?> _
            v4 |> Equal v

        let () =
            let v = box (CustomSerializationTestClassWithProxy12(1, Some 2)) :?> CustomSerializationTestClass<int option>
            let cloner = Cloner.Create(v.GetType())
            let v2 = cloner.Clone(v) :?> _
            v2 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            let v3 = cloner.Clone(v) :?> _
            v3 |> Equal v

            v.OnSerializingWasCalled <- false; v.OnSerializedWasCalled <- false
            let v4 = cloner.Clone(v) :?> _
            v4 |> Equal v

        let () =
            let v = CustomSerializationTestClassWithSimpleProxy()
            let cloner = Cloner.Create(v.GetType())
            let v2 = cloner.Clone(v)
            obj.ReferenceEquals(v, v2) |> False
            v2.GetType() |> Equal (typeof<CustomSerializationTestClassWithSimpleProxyBase>)

        let () =
            let v1 = CustomSerializationTestClassWithProxy12<obj>(1, null)
            let v2 = CustomSerializationTestClassWithProxy12<obj>(2, null)
            v1.Value <- v2
            Cloner.Create(v1.GetType()).Clone(v1) |> Equal (box v1)
            v1.OnSerializingWasCalled <- false; v1.OnSerializedWasCalled <- false
            v2.OnSerializingWasCalled <- false; v2.OnSerializedWasCalled <- false
            v1.Value <- null
            v2.Value <- v1
            Cloner.Create(v2.GetType()).Clone(v2) |> Equal (box v2)
            v1.OnSerializingWasCalled <- false; v1.OnSerializedWasCalled <- false
            v2.OnSerializingWasCalled <- false; v2.OnSerializedWasCalled <- false
            v1.Value <- v2 // creates object graph cycle involving IObjectReferences
            try
                Cloner.Create(v2.GetType()).Clone(v2) |> ignore
                Fail()
            with :? SerializationException -> ()

        let () =
            let v1 = NativeSerializationTestClass<KeyValuePair<obj,obj>>(1, KeyValuePair())
            let v2 = CustomSerializationTestClassWithProxy(2, v1)
            v1.Value <- KeyValuePair<obj,obj>(null, v2)
            try
                Cloner.Create(v1.GetType()).Clone(v1) |> ignore
                Fail()
            with :? SerializationException -> ()

        let () =
            let v1 = NativeSerializationTestClass<obj>(1, null)
            let v2 = CustomSerializationTestClassProxyWithOnDeserialized<obj>(2, v1)
            v1.Value <- v2
            try
                Cloner.Create(v1.GetType()).Clone(v1) |> ignore
                Fail()
            with :? SerializationException -> ()

        let () =
            let v1 = NativeSerializationTestClass<obj>(1, null)
            let mutable v2 = CustomSerializationTestClassProxy2<obj>()
            v2.Id <- 2
            v2.Value <- v1
            v1.Value <- v2
            try
                Cloner.Create(v1.GetType()).Clone(v1) |> ignore
                Fail()
            with :? SerializationException -> ()

        try
            let instance = CustomSerializationTestClassWithInvalidProxy(1, "2")
            Cloner.Create(instance.GetType()).Clone(instance) |> ignore
            Fail()
        with :? SerializationException -> ()

        try
            let instance = CustomSerializationTestClassWithInvalidProxy2(1, "2")
            Cloner.Create(instance.GetType()).Clone(instance) |> ignore
            Fail()
        with :? SerializationException -> ()

        try
            let instance = CustomSerializationTestClassWithInvalidProxy3(1, "2")
            Cloner.Create(instance.GetType()).Clone(instance) |> ignore
            Fail()
        with :? SerializationException -> ()

        try
            let instance = ClassWithNonExistentSerializationProxyClass()
            Cloner.Create(instance.GetType()).Clone(instance) |> ignore
            Fail()
        with :? SerializationException -> ()

    testCustomSerializationCloner()

    let testObjectReferenceHandling() =
        let () =
            let instance = ClassWithObjectReferenceImplementationThatReturnsThis()
            Cloner.Create(instance.GetType()).Clone(instance) |> ignore
            let instance = ClassWithObjectReferenceImplementationThatReturnsThis2()
            Cloner.Create(instance.GetType()).Clone(instance) |> ignore

        try
            let instance = ClassWithBuggyObjectReferenceImplementation1()
            Cloner.Create(instance.GetType()).Clone(instance) |> ignore
            Fail()
        with :? SerializationException -> ()

        try
            let instance = ClassWithBuggyObjectReferenceImplementation2()
            Cloner.Create(instance.GetType()).Clone(instance) |> ignore
            Fail()
        with :? SerializationException -> ()
    testObjectReferenceHandling()


let testCloning() =
    let () =
        let o2 = NativeSerializationTestClassWithUnorderedEvents<obj>(2, null)
        let o1 = NativeSerializationTestClassWithUnorderedEvents<obj>(1, o2)
        let os = [|o2; o1|]
        let cloner = Cloner.Create(os.GetType())
        let os2 = unbox (cloner.Clone(os))
        os2 |> Equal os
        o1.OnSerializedWasCalled |> True
        o1.OnDeserializingWasCalled |> False
        o2.OnSerializedWasCalled |> True
        o2.OnDeserializingWasCalled |> False

    let () =
        let o2 = NativeSerializationTestClass<obj>(1, null)
        let o1 = NativeSerializationTestClass<obj>(2, o2)
        let os = [|o2; o1|]
        let cloner = Cloner.Create(os.GetType())
        onDeserializedList <- []
        let os2 = unbox (cloner.Clone(os))
        os2 |> Equal os
        onDeserializedList |> Equal [2; 1]
        o1.OnSerializedWasCalled |> True
        o1.OnDeserializingWasCalled |> False
        o2.OnSerializedWasCalled |> True
        o2.OnDeserializingWasCalled |> False

    let () =
        // cycle 1
        let o6_2 = NativeSerializationTestClass<obj>(6, null)
        let o5_2 = NativeSerializationTestClass<obj>(5, o6_2)
        let o4_2 = CustomSerializationTestClassWithProxy<obj>(4, o5_2)
        o6_2.Value <- o4_2

        // cycle 2
        let o3_1 = NativeSerializationTestClass2<obj,obj>(3, null, o4_2)
        let o2_1 = NativeSerializationTestClass<obj>(2, o3_1)
        let o1_1 = CustomSerializationTestClassWithProxy<obj>(1, o2_1)
        o3_1.Value <- o1_1

        onDeserializedList <- []
        let cloner = Cloner.Create(o1_1.GetType())
        let o = cloner.Clone(o1_1) :?> CustomSerializationTestClassWithProxy<obj>
        o |> Equal o1_1
        onDeserializedList |> Equal [2;3;5;6] // the order within the strongly connected components
                                              // is implementation defined

    let () =
        let o8 = CustomSerializationTestClassWithProxy<obj>(8, Some(2))
        let o7 = CustomSerializationTestClassWithProxy<obj>(7, Some(1))

        // cycle 1
        let o6_2 = CustomSerializationTestClass<obj>(6, null)
        let o5_2 = CustomSerializationTestClass<obj>(5, (o6_2, box o7, box o8))
        let o4_2 = CustomSerializationTestClass<obj>(4, o5_2)
        o6_2.Value <- o4_2

        // cycle 2
        let o3_1 = NativeSerializationTestClass2<obj,obj>(3, null, o4_2)
        let o2_1 = NativeSerializationTestClass<obj>(2, o3_1)
        let o1_1 = NativeSerializationTestClass<obj>(1, o2_1)
        o3_1.Value <- o1_1

        let o0 = CustomSerializationTestClass<obj>(0, o1_1)

        onDeserializedList <- []
        let os = [|o8; o7; o5_2; o1_1; o0|] : obj[]
        let cloner = Cloner.Create(os.GetType())
        let os2 = cloner.Clone(os) :?> obj[]
        os2.[4] |> Equal os.[4]
        onDeserializedList |> Equal [0;1;2;3;5;6;4] // the order within the strongly connected components
                                                    // is implementation defined

        let reset() =
            onDeserializedList <- []
            o0.OnSerializingWasCalled   <- false; o0.OnSerializedWasCalled <- false
            o1_1.OnSerializingWasCalled <- false; o1_1.OnSerializedWasCalled <- false
            o2_1.OnSerializingWasCalled <- false; o2_1.OnSerializedWasCalled <- false
            o3_1.OnSerializingWasCalled <- false; o3_1.OnSerializedWasCalled <- false
            o4_2.OnSerializingWasCalled <- false; o4_2.OnSerializedWasCalled <- false
            o5_2.OnSerializingWasCalled <- false; o5_2.OnSerializedWasCalled <- false
            o6_2.OnSerializingWasCalled <- false; o6_2.OnSerializedWasCalled <- false
            o7.OnSerializingWasCalled <- false; o7.OnSerializedWasCalled <- false
            o8.OnSerializingWasCalled <- false; o8.OnSerializedWasCalled <- false

        reset()
        let os3 = cloner.Clone(os) :?> obj[]
        os3.[4] |> Equal os.[4]
        onDeserializedList |> Equal [0;1;2;3;5;6;4]

        reset()
        let o = Cloner.Create(o1_1.GetType()).Clone(o1_1)
        onDeserializedList |> Equal [1;2;3;4;5;6]
        o |> Equal (box o1_1)

    try Cloner.Create(typeof<NonSerializableBase>) |> ignore
    with :? System.Runtime.Serialization.SerializationException -> ()
    try Cloner.Create(typeof<string>).Clone(Some "") |> ignore; Fail()
    with :? System.ArgumentException -> ()

let encodingTests() =
    for e in System.Text.Encoding.GetEncodings() do
        let encoding = e.GetEncoding()
        let bs = encoding.GetBytes("test test")
        let decoder = encoding.GetDecoder()
        let cs = Array.zeroCreate 20
        new string(cs, 0, decoder.GetChars(bs, 0, bs.Length, cs, 0)) |> Equal "test test"
        let cloner = FParsec.Cloning.Cloner.Create(decoder.GetType())
        let image = cloner.CaptureImage(decoder)
        let decoder2 = image.CreateClone() :?> System.Text.Decoder
        new string(cs, 0, decoder2.GetChars(bs, 0, bs.Length, cs, 0)) |> Equal "test test"


let run() =
    testStronglyConnectedComponents()
    testComputeTopologicalOrder()
    testCloningEventHandlers()
    testGetSerializedFields()
    testCreateFieldValuesGetter()
    testCreateFieldValuesSetter()
    testCreateISerializableConstructorCaller()
    testCloners()
    testCloning()
    encodingTests()

#endif
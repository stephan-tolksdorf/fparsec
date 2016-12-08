// Copyright (c) Stephan Tolksdorf 2008-2011
// License: Simplified BSD License. See accompanying documentation.

using System;

using Microsoft.FSharp.Core;
using System.Diagnostics;
using System.Collections.Generic;

namespace FParsec {

public enum Associativity {
    None  = 0,
    Left  = 1,
    Right = 2
}


public enum OperatorType {
    Infix   = 0,
    Prefix  = 1,
    Postfix = 2
}


public class Operator<TTerm, TAfterString, TUserState> {
    public OperatorType Type { get; private set; }

    public string String { get; protected set; }
    internal FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> AfterStringParser { get; private set; }

    public string TernaryRightString { get; protected set; }
    internal FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> AfterTernaryRightStringParser { get; private set; }
    public bool IsTernary { get { return TernaryRightString != null; } }

    public int Precedence { get; protected set; }
    public Associativity Associativity { get; protected set; }
    public bool IsAssociative { get { return Associativity != Associativity.None; } }

    internal OptimizedClosures.FSharpFunc<TAfterString, TTerm, TTerm> Mapping1 { get; private set; }
    internal OptimizedClosures.FSharpFunc<TAfterString, TTerm, TTerm, TTerm> Mapping2 { get; private set; }
    internal OptimizedClosures.FSharpFunc<TAfterString, TAfterString, TTerm, TTerm, TTerm, TTerm> Mapping3 { get; private set; }

    private Operator() {}
    static readonly internal Operator<TTerm, TAfterString, TUserState> ZeroPrecedenceOperator = new Operator<TTerm,TAfterString,TUserState>{Type = OperatorType.Prefix};

    private Operator(OperatorType type,
                     string operatorString,
                     FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterStringParser,
                     int precedence)
    {
        Debug.Assert(type >= OperatorType.Infix && type <= OperatorType.Postfix);
        Type = type;
        if (string.IsNullOrEmpty(operatorString)) throw new ArgumentException("operatorString", "The operator string must not be empty.");
        String = operatorString;
        if (afterStringParser == null) throw new ArgumentNullException("afterStringParser");
        AfterStringParser = afterStringParser;
        if (precedence < 1) throw new ArgumentOutOfRangeException("precedence", "The operator precedence must be greater than 0.");
        Precedence = precedence;
    }

    internal Operator(string operatorString,
                      FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterStringParser,
                      int precedence,
                      Associativity associativity,
                      FSharpFunc<TAfterString, FSharpFunc<TTerm, FSharpFunc<TTerm, TTerm>>> mapping)
            : this(OperatorType.Infix, operatorString, afterStringParser, precedence)
    {
        if (associativity < Associativity.None || associativity > Associativity.Right)
            throw new ArgumentOutOfRangeException("associativity", "The associativity argument is invalid.");
        Associativity = associativity;
        if (mapping == null) throw new ArgumentNullException("mapping");
        Mapping2 = OptimizedClosures.FSharpFunc<TAfterString, TTerm, TTerm, TTerm>.Adapt(mapping);
    }

    internal Operator(OperatorType type,
                      string operatorString,
                      FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterStringParser,
                      int precedence,
                      bool isAssociative,
                      FSharpFunc<TAfterString, FSharpFunc<TTerm, TTerm>> mapping)
            : this(type, operatorString, afterStringParser, precedence)
    {
        Debug.Assert(type == OperatorType.Prefix || type == OperatorType.Postfix);
        Associativity = !isAssociative ? Associativity.None :
                        type == OperatorType.Prefix ? Associativity.Right : Associativity.Left;
        if (mapping == null) throw new ArgumentNullException("mapping");
        Mapping1 = OptimizedClosures.FSharpFunc<TAfterString, TTerm, TTerm>.Adapt(mapping);
    }


    internal Operator(string leftString,
                      FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterLeftStringParser,
                      string rightString,
                      FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterRightStringParser,
                      int precedence,
                      Associativity associativity,
                      FSharpFunc<TAfterString, FSharpFunc<TAfterString, FSharpFunc<TTerm, FSharpFunc<TTerm, FSharpFunc<TTerm, TTerm>>>>> mapping)
    {
        Type = OperatorType.Infix;
        if (string.IsNullOrEmpty(leftString)) throw new ArgumentException("leftString", "The operator strings must not be empty.");
        String = leftString;
        if (afterLeftStringParser == null) throw new ArgumentNullException("afterLeftStringParser");
        AfterStringParser = afterLeftStringParser;
        if (string.IsNullOrEmpty(rightString)) throw new ArgumentException("rightString", "The operator strings must not be empty.");
        TernaryRightString = rightString;
        if (afterRightStringParser == null) throw new ArgumentNullException("afterRightStringParser");
        AfterTernaryRightStringParser = afterRightStringParser;
        if (precedence < 1) throw new ArgumentOutOfRangeException("precedence", "The operator precedence must be greater than 0.");
        Precedence = precedence;
        if (associativity < Associativity.None || associativity > Associativity.Right)
            throw new ArgumentOutOfRangeException("associativity", "The associativity argument is invalid.");
        Associativity = associativity;
        if (mapping == null) throw new ArgumentNullException("mapping");
        Mapping3 = OptimizedClosures.FSharpFunc<TAfterString, TAfterString, TTerm, TTerm, TTerm, TTerm>.Adapt(mapping);
    }

    protected class NoAfterStringUnaryMappingAdapter
                    : OptimizedClosures.FSharpFunc<TAfterString, TTerm, TTerm>
    {
        private FSharpFunc<TTerm, TTerm> Mapping;
        public NoAfterStringUnaryMappingAdapter(FSharpFunc<TTerm, TTerm> mapping) { Mapping = mapping; }
        public override TTerm Invoke(TAfterString afterString, TTerm term) { return Mapping.Invoke(term); }
    }

    protected class NoAfterStringBinaryMappingAdapter
                     : OptimizedClosures.FSharpFunc<TAfterString, TTerm, TTerm, TTerm>
    {
        private OptimizedClosures.FSharpFunc<TTerm, TTerm, TTerm> Mapping;
        public NoAfterStringBinaryMappingAdapter(OptimizedClosures.FSharpFunc<TTerm, TTerm, TTerm> mapping) { Mapping = mapping; }
        public override TTerm Invoke(TAfterString afterString, TTerm leftTerm, TTerm rightTerm) {
            return Mapping.Invoke(leftTerm, rightTerm);
        }
    }

    protected class NoAfterStringTernaryMappingAdapter
                    : OptimizedClosures.FSharpFunc<TAfterString, TAfterString, TTerm, TTerm, TTerm, TTerm>
    {
        private OptimizedClosures.FSharpFunc<TTerm, TTerm, TTerm, TTerm> Mapping;
        public NoAfterStringTernaryMappingAdapter(OptimizedClosures.FSharpFunc<TTerm, TTerm, TTerm, TTerm> mapping) { Mapping = mapping; }
        public override TTerm Invoke(TAfterString afterLeftString, TAfterString afterRightString,
                                     TTerm leftTerm, TTerm middleTerm, TTerm rightTerm)
        {
            return Mapping.Invoke(leftTerm, middleTerm, rightTerm);
        }
    }

}

public sealed class InfixOperator<TTerm, TAfterString, TUserState> : Operator<TTerm, TAfterString, TUserState> {
    public InfixOperator(string operatorString,
                         FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterStringParser,
                         int precedence,
                         Associativity associativity,
                         FSharpFunc<TTerm, FSharpFunc<TTerm, TTerm>> mapping)
           : base(operatorString, afterStringParser, precedence, associativity,
                  mapping == null ? null : new NoAfterStringBinaryMappingAdapter(OptimizedClosures.FSharpFunc<TTerm, TTerm, TTerm>.Adapt(mapping))) {}

    public InfixOperator(string operatorString,
                         FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterStringParser,
                         int precedence,
                         Associativity associativity,
                         Unit dummy, // disambiguates overloads in F#
                         FSharpFunc<TAfterString, FSharpFunc<TTerm, FSharpFunc<TTerm, TTerm>>> mapping)
           : base(operatorString, afterStringParser, precedence, associativity, mapping) {}
}

public sealed class PrefixOperator<TTerm, TAfterString, TUserState> : Operator<TTerm, TAfterString, TUserState> {
    public PrefixOperator(string operatorString,
                          FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterStringParser,
                          int precedence,
                          bool isAssociative,
                          FSharpFunc<TTerm, TTerm> mapping)
           : base(OperatorType.Prefix, operatorString, afterStringParser, precedence, isAssociative,
                  mapping == null ? null : new NoAfterStringUnaryMappingAdapter(mapping)) {}

    public PrefixOperator(string operatorString,
                          FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterStringParser,
                          int precedence,
                          bool isAssociative,
                          Unit dummy, // disambiguates overloads in F#
                          FSharpFunc<TAfterString, FSharpFunc<TTerm, TTerm>> mapping)
           : base(OperatorType.Prefix, operatorString, afterStringParser, precedence, isAssociative, mapping) {}
}

 public sealed class PostfixOperator<TTerm, TAfterString, TUserState> : Operator<TTerm, TAfterString, TUserState> {
    public PostfixOperator(string operatorString,
                           FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterStringParser,
                           int precedence,
                           bool isAssociative,
                           FSharpFunc<TTerm, TTerm> mapping)
           : base(OperatorType.Postfix, operatorString, afterStringParser, precedence, isAssociative,
                  mapping == null ? null : new NoAfterStringUnaryMappingAdapter(mapping)) {}

     public PostfixOperator(string operatorString,
                           FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterStringParser,
                           int precedence,
                           bool isAssociative,
                           Unit dummy, // disambiguates overloads in F#
                           FSharpFunc<TAfterString, FSharpFunc<TTerm, TTerm>> mapping)
           : base(OperatorType.Postfix, operatorString, afterStringParser, precedence, isAssociative, mapping) {}
}

public sealed class TernaryOperator<TTerm, TAfterString, TUserState> : Operator<TTerm, TAfterString, TUserState> {
    public TernaryOperator(string leftString,
                           FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterLeftStringParser,
                           string rightString,
                           FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterRightStringParser,
                           int precedence,
                           Associativity associativity,
                           FSharpFunc<TTerm, FSharpFunc<TTerm, FSharpFunc<TTerm, TTerm>>> mapping)
           : base(leftString, afterLeftStringParser, rightString, afterRightStringParser, precedence, associativity,
                  mapping == null ? null : new NoAfterStringTernaryMappingAdapter(OptimizedClosures.FSharpFunc<TTerm, TTerm, TTerm, TTerm>.Adapt(mapping))) {}

    public TernaryOperator(string leftString,
                           FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterLeftStringParser,
                           string rightString,
                           FSharpFunc<CharStream<TUserState>, Reply<TAfterString>> afterRightStringParser,
                           int precedence,
                           Associativity associativity,
                           Unit dummy, // disambiguates overloads in F#
                           FSharpFunc<TAfterString, FSharpFunc<TAfterString, FSharpFunc<TTerm, FSharpFunc<TTerm, FSharpFunc<TTerm, TTerm>>>>> mapping)
           : base(leftString, afterLeftStringParser, rightString, afterRightStringParser, precedence, associativity, mapping) {}
}


public class OperatorPrecedenceParser<TTerm, TAfterString, TUserState> : FSharpFunc<CharStream<TUserState>, Reply<TTerm>> {

    internal struct OperatorData { // declared as struct, so we can allocate it on the stack
        internal Operator<TTerm, TAfterString, TUserState> Operator;
        internal TAfterString AfterStringValue;
        internal CharStreamIndexToken IndexToken;
        internal long Line;
        internal long LineBegin;
    }

    /// <summary>The length of LhsOps and RhsOps. Must be a power of 2.</summary>
    internal const int OpsArrayLength = 128;

    // LhsOps and RhsOps are arrays of operator arrays. LhsOps contains the prefix
    // operator definitions, RhsOps contains all other operator definitions.
    // Both have a fixed size of OpsArrayLength (which must be a power of 2).
    // All operators beginning with the same char modulo OpsArrayLength are
    // grouped together in the same inner array. The inner arrays are sorted
    // by the Operator.String property in descending lexical order.
    // The index of an inner array in the outer array is given by the
    // inner array's operator strings' first char modulo oppArrayLength.
    // An empty inner array is represended by null.

    private readonly Operator<TTerm, TAfterString, TUserState>[][] LhsOps = new Operator<TTerm, TAfterString, TUserState>[OpsArrayLength][];
    private readonly Operator<TTerm, TAfterString, TUserState>[][] RhsOps = new Operator<TTerm, TAfterString, TUserState>[OpsArrayLength][];

    // initialized to 0
    private int PrefixOpCount;
    private int InfixOpCount;
    private int PostfixOpCount;

    private ErrorMessageList ExpectedInfixOrPostfixOperator; // initialized to null

    private readonly Dictionary<string, Operator<TTerm, TAfterString, TUserState>> Reserved = new Dictionary<string, Operator<TTerm, TAfterString, TUserState>>();

    // The following two members aren't static because accessing static members of generic types is rather expensive.

    /// <summary>ParsePrefixOp returns this value to signal that it backtracked and we should try to parse a term.</summary>
    private readonly Operator<TTerm, TAfterString, TUserState> ErrorOp = Operator<TTerm, TAfterString, TUserState>.ZeroPrecedenceOperator;

    /// <summary>Can not be readonly because it is passed as as a ref (for performance reasons), but it is never mutated.</summary>
    private OperatorData ZeroPrecedenceOperatorData = new OperatorData{Operator = Operator<TTerm, TAfterString, TUserState>.ZeroPrecedenceOperator};

    public FSharpFunc<CharStream<TUserState>, Reply<TTerm>> TermParser { get; set; }

    public FSharpFunc<
               Tuple<Position, Position, TernaryOperator<TTerm, TAfterString, TUserState>, TAfterString>,
               ErrorMessageList>
           MissingTernary2ndStringErrorFormatter { get; set; }


    // C# really needs type abbreviations (or better type inference)
    private OptimizedClosures.FSharpFunc<
                Tuple<Position, Operator<TTerm, TAfterString, TUserState>, TAfterString>,
                Tuple<Position, Operator<TTerm, TAfterString, TUserState>, TAfterString>,
                ErrorMessageList>
            _OperatorConflictErrorFormatter;
    public FSharpFunc<
               Tuple<Position, Operator<TTerm, TAfterString, TUserState>, TAfterString>,
               FSharpFunc<Tuple<Position, Operator<TTerm, TAfterString, TUserState>, TAfterString>,
                          ErrorMessageList>>
           OperatorConflictErrorFormatter {
                get { return _OperatorConflictErrorFormatter; }
                set { _OperatorConflictErrorFormatter = OptimizedClosures.FSharpFunc<Tuple<Position, Operator<TTerm, TAfterString, TUserState>, TAfterString>,Tuple<Position, Operator<TTerm, TAfterString, TUserState>, TAfterString>, ErrorMessageList>
                                                        .Adapt(value); }
           }

    public OperatorPrecedenceParser() {
        MissingTernary2ndStringErrorFormatter = new DefaultMissingTernary2ndStringErrorFormatter();
        OperatorConflictErrorFormatter = new DefaultOperatorConflictErrorFormatter();
    }

    public FSharpFunc<CharStream<TUserState>, Reply<TTerm>> ExpressionParser { get { return this; } }

    private bool FindPosition(Operator<TTerm, TAfterString, TUserState>[][] ops, string str, out int arrayIndex, out int indexInArray) {
        var c0 = str[0];
        int i = c0 & (OpsArrayLength - 1);
        arrayIndex = i;
        var array = ops[i];
        int c = -1;
        int j = 0;
        if (array != null) {
            for (j = 0; j < array.Length; ++j) {
                c = String.CompareOrdinal(str, array[j].String);
                if (c >= 0) break;
            }
        }
        indexInArray = j;
        return c == 0;
    }

    private void ThrowDefinitionConflictException(Operator<TTerm, TAfterString, TUserState> op,
                                                  Operator<TTerm, TAfterString, TUserState> oldOp)
    {
        throw new ArgumentException("The definition of the " + op.ToString() + " conflicts with (or duplicates) the previous definition of the " + oldOp.ToString() + ".");
    }

    public void AddOperator(Operator<TTerm, TAfterString, TUserState> op) {
        Operator<TTerm, TAfterString, TUserState> oldOp;
        if (   Reserved.TryGetValue(op.String, out oldOp)
            || (op.IsTernary && Reserved.TryGetValue(op.TernaryRightString, out oldOp)))
        {
            ThrowDefinitionConflictException(op, oldOp);
        }
        var ops = op.Type == OperatorType.Prefix ? LhsOps : RhsOps;
        int i, j;
        if (FindPosition(ops, op.String, out i, out j))
            ThrowDefinitionConflictException(op, ops[i][j]);
        if (op.IsTernary) {
            int i2, j2;
            // make sure the Ternary2ndString isn't registered as an operator
            if (FindPosition(LhsOps, op.TernaryRightString, out i2, out j2))
                ThrowDefinitionConflictException(op, LhsOps[i2][j2]);
            if (FindPosition(RhsOps, op.TernaryRightString, out i2, out j2))
                ThrowDefinitionConflictException(op, RhsOps[i2][j2]);
            Reserved.Add(op.TernaryRightString, op);
        }
        var array = ops[i];
        if (array == null) {
            ops[i] = new Operator<TTerm,TAfterString,TUserState>[1]{op};
        } else {
            int n = array.Length;
            var newArray = new Operator<TTerm,TAfterString,TUserState>[n + 1];
            if (j != 0) Array.Copy(array, 0, newArray, 0, j);
            newArray[j] = op;
            if (j != n) Array.Copy(array, j, newArray, j + 1, n - j);
            ops[i] = newArray;
        }
        if (op.Type == OperatorType.Infix) {
            ++InfixOpCount;
            if (InfixOpCount == 1) {
                ExpectedInfixOrPostfixOperator = PostfixOpCount == 0
                                                 ? Errors.ExpectedInfixOperator
                                                 : Errors.ExpectedInfixOrPostfixOperator;
            }
        } else if (op.Type == OperatorType.Postfix) {
            ++PostfixOpCount;
            if (PostfixOpCount == 1) {
                ExpectedInfixOrPostfixOperator = InfixOpCount == 0
                                                 ? Errors.ExpectedPostfixOperator
                                                 : Errors.ExpectedInfixOrPostfixOperator;
            }
        } else ++PrefixOpCount;
    }

    public bool RemoveInfixOperator(string opString) { return Remove(OperatorType.Infix, opString); }
    public bool RemovePrefixOperator(string opString) { return Remove(OperatorType.Prefix, opString); }
    public bool RemovePostfixOperator(string opString) { return Remove(OperatorType.Postfix, opString); }
    public bool RemoveTernaryOperator(string opStringLeft, string opStringRight) {
        Operator<TTerm,TAfterString,TUserState> reservedOp;
        if (!Reserved.TryGetValue(opStringRight, out reservedOp) || opStringLeft != reservedOp.String) return false;
        Reserved.Remove(opStringRight);
        return Remove(OperatorType.Infix, opStringLeft);
    }

    public bool RemoveOperator(Operator<TTerm,TAfterString,TUserState> op) {
        var ops = op.Type == OperatorType.Prefix ? LhsOps : RhsOps;
        int i, j;
        if (!FindPosition(ops, op.String, out i, out j)) return false;
        if (op != ops[i][j]) return false;
        return op.IsTernary ? RemoveTernaryOperator(op.String, op.TernaryRightString)
                            : Remove(op.Type, op.String);
    }

    private bool Remove(OperatorType operatorType, string opString) {
        var ops = operatorType == OperatorType.Prefix ? LhsOps : RhsOps ;
        int i, j;
        if (!FindPosition(ops, opString, out i, out j)) return false;
        var array = ops[i];
        var n = array.Length;
        if (n == 1) ops[i] = null;
        else {
            var newArray = new Operator<TTerm,TAfterString,TUserState>[n - 1];
            if (j != 0) Array.Copy(array, 0, newArray, 0, j);
            if (j + 1 != n) Array.Copy(array, j + 1, newArray, j, n - j - 1);
            ops[i] = newArray;
        }
        if (operatorType == OperatorType.Infix) {
            --InfixOpCount;
            if (InfixOpCount == 0) {
                ExpectedInfixOrPostfixOperator =  PostfixOpCount == 0 ? null : Errors.ExpectedPostfixOperator;
            }
        } else if (operatorType == OperatorType.Postfix) {
            --PostfixOpCount;
            if (PostfixOpCount == 0) {
                ExpectedInfixOrPostfixOperator = InfixOpCount == 0 ? null : Errors.ExpectedInfixOperator;
            }
        } else --PrefixOpCount;
        return true;
    }

    public IEnumerable<Operator<TTerm, TAfterString, TUserState>> Operators { get {
        var result = new Operator<TTerm, TAfterString, TUserState>[PrefixOpCount + InfixOpCount + PostfixOpCount];
        var n = 0;
        if (PrefixOpCount != 0) {
            foreach (var array in LhsOps)
                if (array != null)
                    foreach (var op in array)
                        result[n++] = op;
        }
        if ((InfixOpCount | PostfixOpCount) != 0) {
            foreach (var array in RhsOps)
                if (array != null)
                    foreach (var op in array)
                        result[n++] = op;
        }
        Debug.Assert(n == result.Length);
        return result;
    } }

    private
      Operator<TTerm, TAfterString, TUserState>
        PeekOp(CharStream<TUserState> stream, Operator<TTerm, TAfterString, TUserState>[][] ops)
    {
        var cs = stream.Peek2();
        var c1 = cs.Char1;
        var c0 = cs.Char0;
        var array = ops[c0 & (OpsArrayLength - 1)];
        if (array != null) {
            foreach (var op in array) {
                var s = op.String;
                if (s[0] == c0) {
                    if (   s.Length <= 1
                        || (s[1] == c1 && (s.Length == 2 || stream.Match(s)))) return op;
                } else if (s[0] < c0) break;
            }
        }
        return null;
    }

    public override Reply<TTerm> Invoke(CharStream<TUserState> stream) {
        Reply<TTerm> reply = new Reply<TTerm>();
        reply.Status = ReplyStatus.Ok;
        var nextOp = ParseExpression(ref ZeroPrecedenceOperatorData, ref reply, stream);
        Debug.Assert(nextOp == null);
        return reply;
    }

    // =============================================================================
    // NOTE: The main complication in the below code arises from the handling of the
    // backtracking related to the after-string-parser. Please see the reference
    // documentation for an explanation of the after-string-parser behaviour.
    // =============================================================================

    internal
      Operator<TTerm, TAfterString, TUserState>
        ParseExpression(ref OperatorData prevOpData, // prevOpData is passed as ref for performance reasons, but is not mutated
                        ref Reply<TTerm> reply,
                        CharStream<TUserState> stream)
    {
        Operator<TTerm, TAfterString, TUserState> op;
        if (PrefixOpCount != 0 && ((op = PeekOp(stream, LhsOps)) != null)) {
            op = ParsePrefixOp(ref prevOpData, op, ref reply, stream);
            // ParsePrefixOp returns ErrorOp when it backtracks and we should try to parse a term
            if (op == null) goto Break;
            if (op != ErrorOp) goto CheckNextOp;
        }
        var error = reply.Error;
        var stateTag = stream.StateTag;
        reply = TermParser.Invoke(stream); // <-- this is where we parse the terms
        if (stateTag == stream.StateTag) {
            error = ErrorMessageList.Merge(error, reply.Error);
            if (PrefixOpCount != 0) error = ErrorMessageList.Merge(error, Errors.ExpectedPrefixOperator);
            reply.Error = error;
        }
        if (reply.Status != ReplyStatus.Ok) goto ReturnNull;
        op = PeekOp(stream, RhsOps);
    CheckNextOp:
        if (op != null) {
            var prevOp = prevOpData.Operator;
            if (prevOp.Precedence > op.Precedence) goto Break;
            if (prevOp.Precedence < op.Precedence) goto Continue;
            // prevOp.Precedence == op.Precedence
            if (op.Type == OperatorType.Infix) {
                var assoc = prevOp.Associativity & op.Associativity;
                if (assoc == Associativity.Left || prevOp.Type == OperatorType.Prefix) goto Break;
                if (assoc == Associativity.Right) goto Continue;
            } else  {
                if (prevOp.Type == OperatorType.Infix) goto Continue;
                Debug.Assert(prevOp.Type == OperatorType.Prefix && op.Type == OperatorType.Postfix);
                if ((prevOp.Associativity | op.Associativity) != Associativity.None) goto Break;
            }
            HandlePossibleConflict(ref prevOpData, op, ref reply, stream);
        } else {
            error = ErrorMessageList.Merge(reply.Error, ExpectedInfixOrPostfixOperator);
            reply.Error = error;
        }
    ReturnNull:
        op = null;
    Break:
        return op;
    Continue:
        return ParseExpressionContinue(ref prevOpData, op, ref reply, stream);
    }

    /// <summary>Parses the following prefix operators, plus the expression the operators apply to.</summary>
    private
      Operator<TTerm, TAfterString, TUserState>
        ParsePrefixOp(ref OperatorData prevOpData,
                      Operator<TTerm, TAfterString, TUserState> op,
                      ref Reply<TTerm> reply,
                      CharStream<TUserState> stream)
    {
        var opData = new OperatorData();
        opData.Line = stream.Line;
        opData.LineBegin = stream.LineBegin;
        opData.IndexToken = stream.IndexToken;
        opData.Operator = op;
        var userState = stream.UserState;
    #if DEBUG
        var ok = stream.Skip(op.String);
        Debug.Assert(ok);
    #else
        stream.Skip((uint)op.String.Length);
    #endif
        var stateTag = stream.StateTag;
        var asReply = op.AfterStringParser.Invoke(stream);
        if (asReply.Status == ReplyStatus.Ok) {
            opData.AfterStringValue = asReply.Result;
            var prevOp = prevOpData.Operator;
            if (   prevOp.Precedence != op.Precedence
                || prevOp.Type != OperatorType.Prefix
                || (prevOp.Associativity | op.Associativity) != Associativity.None)
            {
                reply.Error = asReply.Error;
                var nextOp = ParseExpression(ref opData, ref reply, stream);
                if (reply.Status == ReplyStatus.Ok)
                    reply.Result = op.Mapping1.Invoke(opData.AfterStringValue, reply.Result);
                return nextOp;
            }
            // backtrack to the beginning of the operator
            stream.Seek(opData.IndexToken);
            stream.SetLine_WithoutCheckAndWithoutIncrementingTheStateTag(opData.Line);
            stream.SetLineBegin_WithoutCheckAndWithoutIncrementingTheStateTag(opData.LineBegin);
            stream.UserState = userState;
            stream.StateTag = stateTag - 1;
            ReportConflict(ref prevOpData, op, asReply.Result, ref reply, stream);
            return null;
        } else if (asReply.Status == ReplyStatus.Error && stateTag == stream.StateTag) {
            // backtrack to the beginning of the operator
            stream.Seek(opData.IndexToken);
            stream.StateTag = stateTag - 1;
            return ErrorOp;
        } else {
            reply.Error  = asReply.Error;
            reply.Status = asReply.Status;
            return null;
        }
    }

    /// <summary>Parses (higher-precedence) infix and postfix operators after the first term, together with the argument expressions.</summary>
    private
      Operator<TTerm, TAfterString, TUserState>
        ParseExpressionContinue(ref OperatorData prevOpData,
                                Operator<TTerm, TAfterString, TUserState> op,
                                ref Reply<TTerm> reply,
                                CharStream<TUserState> stream)
    {
        var opData = new OperatorData();
        for (;;) {
            opData.Line = stream.Line;
            opData.LineBegin = stream.LineBegin;
            opData.IndexToken = stream.IndexToken;
            opData.Operator = op;
        #if DEBUG
            var ok = stream.Skip(op.String);
            Debug.Assert(ok);
        #else
            stream.Skip((uint)op.String.Length);
        #endif
            var stateTag = stream.StateTag;
            var asReply = op.AfterStringParser.Invoke(stream);
            if (asReply.Status == ReplyStatus.Ok) {
                opData.AfterStringValue = asReply.Result;
                reply.Error = asReply.Error;
                if (op.Type == OperatorType.Infix) {
                    var result1 = reply.Result;
                    if (!op.IsTernary) {
                        var nextOp = ParseExpression(ref opData, ref reply, stream);
                        if (reply.Status == ReplyStatus.Ok)
                            reply.Result = op.Mapping2.Invoke(opData.AfterStringValue, result1, reply.Result);
                        op = nextOp;
                        if (op == null) break;
                        goto CheckNextOp;
                    } else {
                        ParseExpression(ref ZeroPrecedenceOperatorData, ref reply, stream);
                        if (reply.Status != ReplyStatus.Ok) goto ReturnNull;
                        var result2 = reply.Result;
                        if (stream.Skip(op.TernaryRightString)) {
                            stateTag = stream.StateTag;
                            asReply = op.AfterTernaryRightStringParser.Invoke(stream);
                            if (asReply.Status == ReplyStatus.Ok) {
                                reply.Error = asReply.Error;
                                var nextOp = ParseExpression(ref opData, ref reply, stream);
                                if (reply.Status == ReplyStatus.Ok)
                                    reply.Result = op.Mapping3.Invoke(opData.AfterStringValue, asReply.Result, result1, result2, reply.Result);
                                op = nextOp;
                                if (op == null) break;
                                goto CheckNextOp;
                            } else if (asReply.Status != ReplyStatus.Error || stateTag != stream.StateTag) {
                                reply.Error = asReply.Error;
                                reply.Status = asReply.Status;
                                goto ReturnNull;
                            } else {
                                // backtrack
                                stream.Skip(-op.TernaryRightString.Length);
                                stream.StateTag -= 2;
                            }
                        }
                        HandleMissingTernary2ndStringError(ref opData, ref reply, stream);
                        goto ReturnNull;
                    }
                } else {
                    Debug.Assert(op.Type == OperatorType.Postfix);
                    reply.Result = op.Mapping1.Invoke(opData.AfterStringValue, reply.Result);
                    var lastOp = op;
                    op = PeekOp(stream, RhsOps);
                    // we check for adjacent postfix operators here ...
                    if (op != null) {
                        if (op.Type == OperatorType.Postfix && lastOp.Precedence <= op.Precedence) {
                            if (   lastOp.Precedence < op.Precedence
                                || (lastOp.Associativity | op.Associativity) != Associativity.None) continue;
                            // ... so we can report conflicting postfix operators
                            HandlePossibleConflict(ref opData, op, ref reply, stream);
                            goto ReturnNull;
                        }
                    } else {
                        reply.Error = ErrorMessageList.Merge(reply.Error, ExpectedInfixOrPostfixOperator);
                        break;
                    }
                }
            CheckNextOp:
                var prevOp = prevOpData.Operator;
                if (prevOp.Precedence < op.Precedence) continue;
                if (prevOp.Precedence > op.Precedence) break;
                // prevOp.Precedence == op.Precedence
                if (op.Type == OperatorType.Infix) {
                    var assoc = prevOp.Associativity & op.Associativity;
                    if (assoc == Associativity.Left || prevOp.Type == OperatorType.Prefix) break;
                    if (assoc == Associativity.Right) continue;
                } else { // op.OperatorType == OperatorType.Postfix
                    if (prevOp.Type == OperatorType.Infix) continue;
                    Debug.Assert(prevOp.Type == OperatorType.Prefix);
                    if ((prevOp.Associativity | op.Associativity) != Associativity.None) break;
                }
                HandlePossibleConflict(ref prevOpData, op, ref reply, stream);
            } else { // asReply.Status != ReplyStatus.Ok
                if (asReply.Status == ReplyStatus.Error && stateTag == stream.StateTag) {
                    // backtrack
                    stream.Seek(opData.IndexToken);
                    stream.StateTag -= 2;
                    reply.Error = ErrorMessageList.Merge(reply.Error, ExpectedInfixOrPostfixOperator);
                } else {
                    reply.Error  = asReply.Error;
                    reply.Status = asReply.Status;
                }
            }
        ReturnNull:
            op = null;
            break;
        }
        return op;
    }

    private void HandleMissingTernary2ndStringError(ref OperatorData opData,
                                                    ref Reply<TTerm> reply,
                                                    CharStream<TUserState> stream)
    {
        var firstStringIndex = opData.IndexToken.GetIndex(stream);
        var firstStringColumn = firstStringIndex - opData.LineBegin + 1;
        var firstStringPos = new Position(stream.Name, firstStringIndex, opData.Line, firstStringColumn);
        var secondStringPos = stream.Position;
        var error1 = ExpectedInfixOrPostfixOperator;
        var error2 = MissingTernary2ndStringErrorFormatter.Invoke(Tuple.Create(firstStringPos, secondStringPos, (TernaryOperator<TTerm, TAfterString, TUserState>)opData.Operator, opData.AfterStringValue));
        reply.Error = ErrorMessageList.Merge(reply.Error, ErrorMessageList.Merge(error1, error2));
        reply.Status = ReplyStatus.Error;
    }

    private void HandlePossibleConflict(ref OperatorData prevOpData,
                                        Operator<TTerm, TAfterString, TUserState> op,
                                        ref Reply<TTerm> reply,
                                        CharStream<TUserState> stream)
    {
        // "possible" conflict, because it's not a conflict when the
        // after-string-parser fails without changing the parser state.
        var state = stream.State;
        var ok = stream.Skip(op.String);
        Debug.Assert(ok);
        var stateTag = stream.StateTag;
        var asReply = op.AfterStringParser.Invoke(stream);
        if (asReply.Status == ReplyStatus.Ok) {
            stream.BacktrackTo(ref state);
            ReportConflict(ref prevOpData, op, asReply.Result, ref reply, stream);
        } else if (asReply.Status == ReplyStatus.Error && stateTag == stream.StateTag) {
            // backtrack and ignore the operator
            stream.BacktrackTo(ref state);
            reply.Error = ErrorMessageList.Merge(reply.Error, ExpectedInfixOrPostfixOperator);
        } else {
            // report AfterStringParser error instead of conflict
            reply.Error  = asReply.Error;
            reply.Status = asReply.Status;
        }
    }

    private void ReportConflict(ref OperatorData prevOpData,
                                Operator<TTerm, TAfterString, TUserState> op,
                                TAfterString afterStringValue,
                                ref Reply<TTerm> reply,
                                CharStream<TUserState> stream)
    {
        var prevOpIndex = prevOpData.IndexToken.GetIndex(stream);
        var prevOpColumn = prevOpIndex - prevOpData.LineBegin + 1;
        var prevOpPos = new Position(stream.Name, prevOpIndex, prevOpData.Line, prevOpColumn);
        var error = _OperatorConflictErrorFormatter.Invoke(
                            Tuple.Create(prevOpPos, prevOpData.Operator, prevOpData.AfterStringValue),
                            Tuple.Create(stream.Position, op, afterStringValue));
        reply.Error = ErrorMessageList.Merge(reply.Error, error);
        reply.Status = ReplyStatus.Error;
    }

    private sealed class DefaultMissingTernary2ndStringErrorFormatter
                         : FSharpFunc<Tuple<Position, Position, TernaryOperator<TTerm, TAfterString, TUserState>, TAfterString>, ErrorMessageList>
    {
        public override ErrorMessageList Invoke(Tuple<Position, Position, TernaryOperator<TTerm, TAfterString, TUserState>, TAfterString> value) {
            var position1 = value.Item1;
            var position2 = value.Item2;
            var op = value.Item3;
            return Errors.MissingTernary2ndString(position1, position2, op);
        }
    }

    private sealed class DefaultOperatorConflictErrorFormatter
                        : OptimizedClosures.FSharpFunc<Tuple<Position, Operator<TTerm, TAfterString, TUserState>, TAfterString>,
                                                       Tuple<Position, Operator<TTerm, TAfterString, TUserState>, TAfterString>,
                                                       ErrorMessageList>
    {
        public override ErrorMessageList Invoke(Tuple<Position, Operator<TTerm, TAfterString, TUserState>, TAfterString> arg1, Tuple<Position, Operator<TTerm, TAfterString, TUserState>, TAfterString> arg2) {
            return Errors.OperatorsConflict(arg1.Item1, arg1.Item2, arg2.Item1, arg2.Item2);
        }
    }

}


}
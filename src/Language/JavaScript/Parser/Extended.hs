module Language.JavaScript.Parser.Extended
    ( module Language.JavaScript.Parser
    , stripAnnotations
    ) where

import           Language.JavaScript.Parser
import           Language.JavaScript.Parser.AST

-- Strip out the location info
stripAnnotations :: JSAST -> JSAST
stripAnnotations (JSAstProgram xs _) = JSAstProgram (map sa xs) JSNoAnnot
stripAnnotations (JSAstStatement s _) = JSAstStatement (sa s) JSNoAnnot
stripAnnotations (JSAstExpression e _) = JSAstExpression (sa e) JSNoAnnot
stripAnnotations (JSAstLiteral s _)  = JSAstLiteral (sa s) JSNoAnnot

class NoAnnot a where
    sa :: a -> a

instance NoAnnot JSStatement where
    sa (JSStatementBlock _ xs _ s) = JSStatementBlock JSNoAnnot (map sa xs) JSNoAnnot (sa s)
    sa (JSBreak _ x s) = JSBreak JSNoAnnot (sa x) (sa s)
    sa (JSContinue _ x s) = JSContinue JSNoAnnot (sa x) (sa s)
    sa (JSConstant _ xs s) = JSConstant JSNoAnnot (sa xs) (sa s)
    sa (JSDoWhile _d x1 _w _lb x2 _rb x3) = JSDoWhile JSNoAnnot (sa x1) JSNoAnnot JSNoAnnot (sa x2) JSNoAnnot (sa x3)
    sa (JSFor _ _ x1s _ x2s _ x3s _ x4) = JSFor JSNoAnnot JSNoAnnot (sa x1s) JSNoAnnot (sa x2s) JSNoAnnot (sa x3s) JSNoAnnot (sa x4)
    sa (JSForIn _ _ x1s op x2s _ st) = JSForIn JSNoAnnot JSNoAnnot (sa x1s) (sa op) (sa x2s) JSNoAnnot (sa st)
    sa (JSForVar _ _ _ x1s _ x2s _ x3s _ st) = JSForVar JSNoAnnot JSNoAnnot JSNoAnnot (sa x1s) JSNoAnnot (sa x2s) JSNoAnnot (sa x3s) JSNoAnnot (sa st)
    sa (JSForVarIn _ _ _ x1 op x2 _ x3) = JSForVarIn JSNoAnnot JSNoAnnot JSNoAnnot (sa x1) (sa op) (sa x2) JSNoAnnot (sa x3)
    sa (JSFunction _ n _ pl _ x3 s) = JSFunction JSNoAnnot (sa n) JSNoAnnot (sa pl) JSNoAnnot (sa x3) (sa s)
    sa (JSIf _ _ x1 _rb x2) = JSIf JSNoAnnot JSNoAnnot (sa x1) JSNoAnnot (sa x2)
    sa (JSIfElse _ _lb x1 _rb x2 _e x3) = JSIfElse JSNoAnnot JSNoAnnot (sa x1) JSNoAnnot (sa x2) JSNoAnnot (sa x3)
    sa (JSLabelled x1 _c x2) = JSLabelled (sa x1) JSNoAnnot (sa x2)
    sa (JSEmptyStatement _) = JSEmptyStatement JSNoAnnot
    sa (JSExpressionStatement l s) = JSExpressionStatement (sa l) (sa s)
    sa (JSAssignStatement lhs op rhs s) = JSAssignStatement  (sa lhs) (sa op) (sa rhs) (sa s)
    sa (JSMethodCall e _ a _ s) = JSMethodCall  (sa e) JSNoAnnot (sa a) JSNoAnnot (sa s)
    sa (JSReturn _ (Just me) s) = JSReturn JSNoAnnot (Just (sa me)) (sa s)
    sa (JSReturn _ Nothing s) = JSReturn JSNoAnnot Nothing (sa s)
    sa (JSSwitch _ _ x _ _ x2 _ s) = JSSwitch JSNoAnnot JSNoAnnot (sa x) JSNoAnnot JSNoAnnot (map sa x2) JSNoAnnot (sa s)
    sa (JSThrow _ x s) = JSThrow JSNoAnnot (sa x) (sa s)
    sa (JSTry _ xt1 xtc xtf) = JSTry JSNoAnnot (sa xt1) (map sa xtc) (sa xtf)
    sa (JSVariable _ xs s) = JSVariable JSNoAnnot (sa xs) (sa s)
    sa (JSWhile _ _ x1 _ x2) = JSWhile JSNoAnnot JSNoAnnot (sa x1) JSNoAnnot (sa x2)
    sa (JSWith _ _ x1 _ x s) = JSWith JSNoAnnot JSNoAnnot (sa x1) JSNoAnnot (sa x) (sa s)

instance NoAnnot JSExpression where
    sa (JSArrayLiteral _lb xs _rb) = JSArrayLiteral JSNoAnnot (map sa xs) JSNoAnnot
    sa (JSAssignExpression lhs op rhs) = JSAssignExpression  (sa lhs) (sa op) (sa rhs)
    sa (JSCallExpression ex _ xs _) = JSCallExpression  (sa ex) JSNoAnnot (sa xs) JSNoAnnot
    sa (JSCallExpressionDot ex _os xs) = JSCallExpressionDot  (sa ex) JSNoAnnot (sa xs)
    sa (JSCallExpressionSquare ex _ xs _) = JSCallExpressionSquare  (sa ex) JSNoAnnot (sa xs) JSNoAnnot
    sa (JSDecimal _ s) = JSDecimal JSNoAnnot s
    sa (JSCommaExpression l _ r) = JSCommaExpression  (sa l) JSNoAnnot (sa r)
    sa (JSExpressionBinary x1 op x2) = JSExpressionBinary (sa x1) (sa op) (sa x2)
    sa (JSExpressionParen _lp x _rp) = JSExpressionParen JSNoAnnot (sa x) JSNoAnnot
    sa (JSExpressionPostfix xs op) = JSExpressionPostfix (sa xs) (sa op)
    sa (JSExpressionTernary x1 _q x2 _c x3) = JSExpressionTernary (sa x1) JSNoAnnot (sa x2) JSNoAnnot (sa x3)
    sa (JSFunctionExpression _ n _lb pl _rb x3) = JSFunctionExpression JSNoAnnot (sa n) JSNoAnnot (sa pl) JSNoAnnot (sa x3)
    sa (JSHexInteger _ s) = JSHexInteger JSNoAnnot s
    sa (JSOctal _ s) = JSOctal JSNoAnnot s
    sa (JSIdentifier _ s) = JSIdentifier JSNoAnnot s
    sa (JSLiteral _ s) = JSLiteral JSNoAnnot s
    sa (JSMemberDot x1s _d x2 ) = JSMemberDot (sa x1s) JSNoAnnot (sa x2)
    sa (JSMemberExpression e _ a _) = JSMemberExpression  (sa e) JSNoAnnot (sa a) JSNoAnnot
    sa (JSMemberNew _ n _ s _) = JSMemberNew JSNoAnnot (sa n) JSNoAnnot (sa s) JSNoAnnot
    sa (JSMemberSquare x1s _lb x2 _rb) = JSMemberSquare (sa x1s) JSNoAnnot (sa x2) JSNoAnnot
    sa (JSNewExpression _n e) = JSNewExpression JSNoAnnot (sa e)
    sa (JSObjectLiteral _lb xs _rb) = JSObjectLiteral JSNoAnnot (sa xs) JSNoAnnot
    sa (JSRegEx _ s) = JSRegEx JSNoAnnot s
    sa (JSStringLiteral _ s) = JSStringLiteral JSNoAnnot s
    sa (JSUnaryExpression op x) = JSUnaryExpression (sa op) (sa x)
    sa (JSVarInitExpression x1 x2) = JSVarInitExpression (sa x1) (sa x2)

instance NoAnnot JSTryCatch where
    sa (JSCatch _ _ x1 _ x3) = JSCatch JSNoAnnot JSNoAnnot (sa x1) JSNoAnnot (sa x3)
    sa (JSCatchIf _ _ x1 _ ex _ x3) = JSCatchIf JSNoAnnot JSNoAnnot (sa x1) JSNoAnnot (sa ex) JSNoAnnot (sa x3)

instance NoAnnot JSTryFinally where
    sa (JSFinally _ x) = JSFinally JSNoAnnot (sa x)
    sa JSNoFinally = JSNoFinally

instance NoAnnot JSIdent where
    sa (JSIdentName _ s) = JSIdentName JSNoAnnot s
    sa JSIdentNone = JSIdentNone

instance NoAnnot JSObjectProperty where
    sa (JSPropertyNameandValue x1 _colon x2s) = JSPropertyNameandValue (sa x1) JSNoAnnot (map sa x2s)
    sa (JSPropertyAccessor s x1 _lb1 x2s _rb1 x3) = JSPropertyAccessor (sa s) (sa x1) JSNoAnnot (map sa x2s) JSNoAnnot (sa x3)

instance NoAnnot JSPropertyName where
    sa (JSPropertyIdent _ s) = JSPropertyIdent JSNoAnnot s
    sa (JSPropertyString _ s) = JSPropertyString JSNoAnnot s
    sa (JSPropertyNumber _ s) = JSPropertyNumber JSNoAnnot s

instance NoAnnot JSAccessor where
    sa (JSAccessorGet _) = JSAccessorGet JSNoAnnot

instance NoAnnot JSBlock where
    sa (JSBlock _ xs _) = JSBlock JSNoAnnot (map sa xs) JSNoAnnot

instance NoAnnot JSSwitchParts where
    sa (JSCase _ x1 _c x2s) = JSCase JSNoAnnot (sa x1) JSNoAnnot (map sa x2s)
    sa (JSDefault _ _c xs) = JSDefault JSNoAnnot JSNoAnnot (map sa xs)

instance NoAnnot JSBinOp where
    sa (JSBinOpAnd _) = JSBinOpAnd JSNoAnnot
    sa (JSBinOpBitAnd _) = JSBinOpBitAnd JSNoAnnot
    sa (JSBinOpBitOr _) = JSBinOpBitOr JSNoAnnot
    sa (JSBinOpBitXor _) = JSBinOpBitXor JSNoAnnot
    sa (JSBinOpDivide _) = JSBinOpDivide JSNoAnnot
    sa (JSBinOpEq _) = JSBinOpEq JSNoAnnot
    sa (JSBinOpGe _) = JSBinOpGe JSNoAnnot
    sa (JSBinOpGt _) = JSBinOpGt JSNoAnnot
    sa (JSBinOpIn _) = JSBinOpIn JSNoAnnot
    sa (JSBinOpInstanceOf _) = JSBinOpInstanceOf JSNoAnnot
    sa (JSBinOpLe _) = JSBinOpLe JSNoAnnot
    sa (JSBinOpLsh _) = JSBinOpLsh JSNoAnnot
    sa (JSBinOpLt _) = JSBinOpLt JSNoAnnot
    sa (JSBinOpMinus _) = JSBinOpMinus JSNoAnnot
    sa (JSBinOpMod _) = JSBinOpMod JSNoAnnot
    sa (JSBinOpNeq _) = JSBinOpNeq JSNoAnnot
    sa (JSBinOpOr _) = JSBinOpOr JSNoAnnot
    sa (JSBinOpPlus _) = JSBinOpPlus JSNoAnnot
    sa (JSBinOpRsh _) = JSBinOpRsh JSNoAnnot
    sa (JSBinOpStrictEq _) = JSBinOpStrictEq JSNoAnnot
    sa (JSBinOpStrictNeq _) = JSBinOpStrictNeq JSNoAnnot
    sa (JSBinOpTimes _) = JSBinOpTimes JSNoAnnot
    sa (JSBinOpUrsh _) = JSBinOpUrsh JSNoAnnot

instance NoAnnot JSUnaryOp where
    sa (JSUnaryOpDecr _) = JSUnaryOpDecr JSNoAnnot
    sa (JSUnaryOpDelete _) = JSUnaryOpDelete JSNoAnnot
    sa (JSUnaryOpIncr _) = JSUnaryOpIncr JSNoAnnot
    sa (JSUnaryOpMinus _) = JSUnaryOpMinus JSNoAnnot
    sa (JSUnaryOpNot _) = JSUnaryOpNot JSNoAnnot
    sa (JSUnaryOpPlus _) = JSUnaryOpPlus JSNoAnnot
    sa (JSUnaryOpTilde _) = JSUnaryOpTilde JSNoAnnot
    sa (JSUnaryOpTypeof _) = JSUnaryOpTypeof JSNoAnnot
    sa (JSUnaryOpVoid _) = JSUnaryOpVoid JSNoAnnot

instance NoAnnot JSAssignOp where
    sa (JSAssign _) = JSAssign JSNoAnnot
    sa (JSTimesAssign _) = JSTimesAssign JSNoAnnot
    sa (JSDivideAssign _) = JSDivideAssign JSNoAnnot
    sa (JSModAssign _) = JSModAssign JSNoAnnot
    sa (JSPlusAssign _) = JSPlusAssign JSNoAnnot
    sa (JSMinusAssign _) = JSMinusAssign JSNoAnnot
    sa (JSLshAssign _) = JSLshAssign JSNoAnnot
    sa (JSRshAssign _) = JSRshAssign JSNoAnnot
    sa (JSUrshAssign _) = JSUrshAssign JSNoAnnot
    sa (JSBwAndAssign _) = JSBwAndAssign JSNoAnnot
    sa (JSBwXorAssign _) = JSBwXorAssign JSNoAnnot
    sa (JSBwOrAssign _) = JSBwOrAssign JSNoAnnot

instance NoAnnot JSVarInitializer where
    sa (JSVarInit _ n) = JSVarInit JSNoAnnot (sa n)
    sa JSVarInitNone = JSVarInitNone

instance NoAnnot JSSemi where
    sa (JSSemi _) = JSSemi JSNoAnnot
    sa JSSemiAuto = JSSemiAuto

instance NoAnnot JSArrayElement where
    sa (JSArrayElement e) = JSArrayElement  (sa e)
    sa (JSArrayComma _) = JSArrayComma JSNoAnnot

instance NoAnnot a => NoAnnot (JSCommaList a) where
    sa (JSLCons xs _ x) = JSLCons (sa xs) JSNoAnnot (sa x)
    sa (JSLOne x) = JSLOne (sa x)
    sa JSLNil = JSLNil

instance NoAnnot a => NoAnnot (JSCommaTrailingList a) where
    sa (JSCTLComma xs _) = JSCTLComma (sa xs) JSNoAnnot
    sa (JSCTLNone xs)    = JSCTLNone (sa xs)

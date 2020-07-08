// Code generated by "stringer -output stringer.go -type=exprMode,opKind"; DO NOT EDIT.

package main

import "strconv"

func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[exprAddrOf-1]
	_ = x[exprBool-2]
	_ = x[exprCondInit-3]
	_ = x[exprCondReturn-4]
	_ = x[exprFunc-5]
	_ = x[exprLValue-6]
	_ = x[exprPSelect-7]
	_ = x[exprSelect-8]
	_ = x[exprValue-9]
	_ = x[exprVoid-10]
}

const _exprMode_name = "exprAddrOfexprBoolexprCondInitexprCondReturnexprFuncexprLValueexprPSelectexprSelectexprValueexprVoid"

var _exprMode_index = [...]uint8{0, 10, 18, 30, 44, 52, 62, 73, 83, 92, 100}

func (i exprMode) String() string {
	i -= 1
	if i < 0 || i >= exprMode(len(_exprMode_index)-1) {
		return "exprMode(" + strconv.FormatInt(int64(i+1), 10) + ")"
	}
	return _exprMode_name[_exprMode_index[i]:_exprMode_index[i+1]]
}
func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[opNormal-0]
	_ = x[opArray-1]
	_ = x[opArrayParameter-2]
	_ = x[opFunction-3]
	_ = x[opUnion-4]
	_ = x[opBitfield-5]
	_ = x[opStruct-6]
}

const _opKind_name = "opNormalopArrayopArrayParameteropFunctionopUnionopBitfieldopStruct"

var _opKind_index = [...]uint8{0, 8, 15, 31, 41, 48, 58, 66}

func (i opKind) String() string {
	if i < 0 || i >= opKind(len(_opKind_index)-1) {
		return "opKind(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _opKind_name[_opKind_index[i]:_opKind_index[i+1]]
}

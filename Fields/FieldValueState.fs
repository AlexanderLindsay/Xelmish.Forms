module Xelmish.Form.Fields.FieldValueState

type FieldValueState<'T>
    = Valid of 'T
    | Invalid of string*string
    | NoValue
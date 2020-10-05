.LUA_HEADER
; VERSION 5.3
; FORMAT 0
; SIZES:
;   int 4
;   size_t 8
;   Instruction 4
;   lua_Integer 8
;   lua_Number 8
; @test_tab.lua:0-0
.instructions
OP_CLOSURE 0 0                ; R(0) := closure(KPROTO[0])
OP_SETTABUP 0 256 0           ; UpValue[0]["foo"] := R(0)
OP_RETURN 0 1 0               ; return R(0), ... , R(-1)
.consts
; CONST "foo"
.upvalues
; UPVALUE 1 0
.protos  
  ; @test_tab.lua:1-6
  .instructions
  OP_SETTABLE 0 1 256           ; R(0)[R(1)] := 42
  OP_SETTABLE 0 256 257         ; R(0)[42] := 43
  OP_SETTABLE 0 257 1           ; R(0)[43] := R(1)
  OP_RETURN 0 2 0               ; return R(0), ... , R(0)
  OP_RETURN 0 1 0               ; return R(0), ... , R(-1)
  .consts
  ; CONST 42
  ; CONST 43
  .upvalues
  .protos


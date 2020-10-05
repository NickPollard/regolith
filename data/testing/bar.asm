.LUA_HEADER
; VERSION 5.3
; FORMAT 0
; SIZES:
;   int 4
;   size_t 8
;   Instruction 4
;   lua_Integer 8
;   lua_Number 8
; @test_add.lua:0-0
.instructions
OP_CLOSURE 0 0                ; R(0) := closure(KPROTO[0])
OP_SETTABUP 0 256 0           ; UpValue[0]["add12"] := R(0)
OP_CLOSURE 0 1                ; R(0) := closure(KPROTO[1])
OP_SETTABUP 0 257 0           ; UpValue[0]["add128"] := R(0)
OP_CLOSURE 0 2                ; R(0) := closure(KPROTO[2])
OP_SETTABUP 0 258 0           ; UpValue[0]["add4327"] := R(0)
OP_RETURN 0 1 0               ; return R(0), ... , R(-1)
.consts
; CONST "add12"
; CONST "add128"
; CONST "add4327"
.upvalues
; UPVALUE 1 0
.protos  
  ; @test_add.lua:1-3
  .instructions
  OP_ADD 1 0 256                ; R(1) = R(0) + 12
  OP_RETURN 1 2 0               ; return R(1), ... , R(1)
  OP_RETURN 0 1 0               ; return R(0), ... , R(-1)
  .consts
  ; CONST 12
  .upvalues
  .protos
  ; @test_add.lua:4-6
  .instructions
  OP_ADD 1 0 256                ; R(1) = R(0) + 128
  OP_RETURN 1 2 0               ; return R(1), ... , R(1)
  OP_RETURN 0 1 0               ; return R(0), ... , R(-1)
  .consts
  ; CONST 128
  .upvalues
  .protos
  ; @test_add.lua:7-9
  .instructions
  OP_ADD 1 0 256                ; R(1) = R(0) + 4327
  OP_RETURN 1 2 0               ; return R(1), ... , R(1)
  OP_RETURN 0 1 0               ; return R(0), ... , R(-1)
  .consts
  ; CONST 4327
  .upvalues
  .protos


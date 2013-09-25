module Template
  ( programTmpl
  , mainTmpl
  , programTmplPrefix
  , programTmplPostfix
  , mainTmplPrefix
  , mainTmplPostfix
  ) where

programTmplPrefix =
  ".section .data\n" ++
  "digit:\n" ++
  "    .ascii \"..........\\n\"\n" ++
  ".section .text\n" ++
  ".global _start\n"

programTmplPostfix =
  "read:\n" ++
  "    pushq %rbx\n" ++
  "    pushq %rcx\n" ++
  "    pushq %rdx\n" ++
  "    pushq $0\n" ++

  "read_loop:\n" ++
  "    movq $3, %rax\n" ++
  "    movq $0, %rbx\n" ++
  "    movq $digit, %rcx\n" ++
  "    movq $0, (%rcx)\n" ++
  "    movq $1, %rdx\n" ++
  "    int $0x80\n" ++

  "    cmpq $10, (%rcx)\n" ++
  "    jz read_loop_end\n" ++
  "    popq %rax\n" ++
  "    movq $10, %rdx\n" ++
  "    mulq %rdx\n" ++
  "    addq (%rcx), %rax\n" ++
  "    subq $48, %rax\n" ++
  "    pushq %rax\n" ++
  "    jmp read_loop\n" ++
  "read_loop_end:\n" ++

  "    popq %rax\n" ++
  "    popq %rdx\n" ++
  "    popq %rcx\n" ++
  "    popq %rbx\n" ++
  "    ret\n" ++

  "write:\n" ++
  "    pushq %rbp\n" ++
  "    movq %rsp, %rbp\n" ++
  "    addq $16, %rbp\n" ++
  "    pushq %rax\n" ++
  "    pushq %rbx\n" ++
  "    pushq %rcx\n" ++
  "    pushq %rdx\n" ++
  "    pushq %rdi\n" ++

  "    movq $digit, %rdi\n" ++
  "    movq $10, %rcx\n" ++
  "write_clear:\n" ++
  "    movb $32, (%rdi)\n" ++
  "    incq %rdi\n" ++
  "    loop write_clear\n" ++

  "    movq (%rbp), %rcx\n" ++
  "    movq $digit, %rdi\n" ++
  "    addq $9, %rdi\n" ++
  "write_loop:\n" ++
  "    xorq %rdx, %rdx\n" ++
  "    movq %rcx, %rax\n" ++
  "    movq $10, %rbx\n" ++
  "    divq %rbx\n" ++
  "    movb %dl, (%rdi)\n" ++
  "    addb $48, (%rdi)\n" ++
  "    decq %rdi\n" ++
  "    movq %rax, %rcx\n" ++
  "    orq %rcx, %rcx\n" ++
  "    jnz write_loop\n" ++

  "    movq $4, %rax\n" ++
  "    movq $1, %rbx\n" ++
  "    incq %rdi\n" ++
  "    movq %rdi, %rcx\n" ++
  "    movq $11, %rdx\n" ++
  "    addq $digit, %rdx\n" ++
  "    subq %rdi, %rdx\n" ++
  "    int $0x80\n" ++

  "    pop %rdi\n" ++
  "    pop %rdx\n" ++
  "    pop %rcx\n" ++
  "    pop %rbx\n" ++
  "    pop %rax\n" ++
  "    popq %rbp\n" ++
  "    ret\n"

programTmpl body =
  programTmplPrefix ++ body ++ programTmplPostfix

mainTmplPrefix =
  "_start:\n"

mainTmplPostfix =
  "popq %rax\n" ++
  "movq $1, %rax\n" ++
  "movq $0, %rbx\n" ++
  "int $0x80\n"

mainTmpl body =
  mainTmplPrefix ++ body ++ mainTmplPostfix

{-# LANGUAGE QuasiQuotes #-}

module AsmTemplates (header, footer) where

import qualified Data.Map as M

import BFMonad
import Quoter


header :: M.Map String String -> BF ()
header = [str|
.data
  read_error: .ascii "error: couldn't read '%s'\n\0"
  flag_error: .ascii "error: invalid flag '%s'\n\0"
  mem_error:  .ascii "error: ran out of space\n\0"
  usage_str:  .ascii "usage: %s [-a] args\n\0"
  print_str:  .ascii "%d\n\0"
  flag_fstr:  .ascii "-%s \0"
  read_fstr:  .ascii "%d \0"
  char_fstr:  .ascii "%c\0"
  read_var:   .int 0

.text
  .global {main}

usage_exit:
    push (%ebp)
    push $usage_str
    {push_stderr}
    call {fprintf}
    add $4,%esp
    call {exit}

read_failed:
    cmp $1,%ecx   # if (failed with 1st arg):
    je read_flag  #   try reading flag

    # else: print error & exit(1)
    push (%ebp,%ecx,4)
    push $read_error
    {push_stderr}
    call {fprintf}
    push $1
    call usage_exit

read_flag:
    # sscanf(argv[1], read_fstr, &read_var);
    push $read_var
    push $flag_fstr
    push 4(%ebp)
    xor %eax,%eax
    call {sscanf}
    add $12,%esp

    # if (flag != "-a"): error out
    cmp $1,%eax
    jne flag_failed
    cmp $97,read_var
    jne flag_failed

    # else: change print_str and continue
    mov char_fstr,%ecx
    mov %ecx,print_str

    jmp read_args_done

flag_failed:
    push 4(%ebp)
    push $flag_error
    {push_stderr}
    call {fprintf}
    push $2
    call usage_exit

init_stack:
    push ${stack_size_bytes}
    call {malloc}
    add $4,%esp
    movw $0, (%eax) # zero bottom
    call null_check
    ret

null_check:
    cmp $0,%eax
    jne return_null_check

    push $mem_error
    {push_stderr}
    call {fprintf}

    push $3
    call {exit}

  return_null_check:
    ret

resize_stack:
    # compute new stack size (double each time)
    sal $1,%edx
    lea (,%edx,4),%esi

    push %eax
    push %ecx
    push %edx

    # resize the stack
    push %esi
    push %ebx
    call {realloc}
    add $8,%esp

    # set base pointer of newly allocated stack
    call null_check
    mov %eax,%ebx

    pop %edx
    pop %ecx
    pop %eax

    jmp do_push_acc

push_acc:
    # check if stack exceeds memory limit & resize in case it does
    inc %ecx
    cmp %edx,%ecx
    jge resize_stack

  do_push_acc:  # push accumulator & return
    mov %eax,(%ebx,%ecx,4)
    ret

pop_add:
    cmp $0,%ecx        # if len(stack) == 0:
    je return_pop_add  #   do nothing

    # else: add ToS to accumulator and decrement index
    add (%ebx,%ecx,4),%eax
    dec %ecx

  return_pop_add:
    ret

{main}:
    # allocate memory for the two stacks
    call init_stack
    push %eax  # keep right stack's bp on stack
    call init_stack

    # we start with left stack
    mov %eax,%ebx
    xor %esi,%esi

    # i = argc-1
    mov  8(%esp),%ecx
    dec %ecx

    mov 12(%esp),%ebp  # argv[0]

    # Loop over argv[N..1], read them and push to stack
    cmp $0,%ecx
    je read_args_done

  read_args:
    push %ecx

    # sscanf(argv[i], read_fstr, &read_var);
    push $read_var
    push $read_fstr
    push (%ebp,%ecx,4)
    xor %eax,%eax
    call {sscanf}

    add $12,%esp
    pop %ecx

    cmp $1, %eax     # if sscanf failed:
    jne read_failed  #   abort

    # else: push read argument to stack & increment index
    mov read_var,%edi
    inc %esi
    mov %edi, (%ebx,%esi,4)

    loop read_args

  read_args_done:

    # initialize left stack
    xor %eax,%eax
    mov %esi,%ecx
    mov ${stack_size},%edx

    # initialize right stack
    push $0
    push ${stack_size}
    mov %esp,%ebp

|]

footer :: M.Map String String -> BF ()
footer = [str|
    # print ToS, decrement index, rinse & repeat
    cmp $0,%ecx
    je print_stack_done

  print_stack:
    push %ecx

    # call printf(print_str, ToS);
    push (%ebx,%ecx,4)
    push $print_str
    call {printf}

    add $8,%esp
    pop %ecx

    loop print_stack

  print_stack_done:
    # clean-up & exit
    push %ebx
    call {free}
    push 8(%ebp)
    call {free}
    push $0
    call {exit}
|]

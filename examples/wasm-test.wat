(module
  (func $Math/fib (param $n i32) (result i32)
    local.get $n
    i32.const 2
    i32.lt_s
    (if (result i32)
      (then local.get $n)
      (else
        local.get $n
        i32.const 1
        i32.sub
        call $Math/fib
        local.get $n
        i32.const 2
        i32.sub
        call $Math/fib
        i32.add)))

  (func (export "_start") (result i32)
    i32.const 8
    call $Math/fib))
(deftype risc-operand-kind ()
  "The operand type supported by an instruction."
  '(member
    :register    ; register operand         - Rn
    :indirect    ; indirect operand         - (Rn)
    :immediate5  ; 5-bit immediate operand  - n
    :immediate32 ; 32-bit immediate operand - n
    :r14-indexed ; indexed operand          - (R14+n)
    :r15-indexed ; indexed operand          - (R15+n)
    :r14-offset  ; offset operand           - (R14+Rn)
    :r15-offset  ; offset operand           - (R15+Rn)
    :pc          ; program counter          - PC
    :unused      ; no operand
    ))

(deftype risc-operand-flags-set ()
  "The CPU flags modified by an instruction."
  '(member :cpu-flag-carry :cpu-flag-zero :cpu-flag-negative))

(defstruct risc-operand
  "A CPU instruction operand."
  (kind nil :type risc-operand-kind))

(defconstant REGISTER-NOT-USED -1)
(defconstant FLAGS-NOT-SET -1)
(defconstant OPERAND-NOT-USED (make-risc-operand :kind :unused))

(defstruct risc-instruction
  (mnemonic nil :type string)
  (opcode nil :type (integer 0 63))
  (supported-by-gpu t :type atom)
  (supported-by-dsp t :type atom)
  (cycles-to-read-source-register REGISTER-NOT-USED :type integer)
  (cycles-to-read-destination-register REGISTER-NOT-USED :type integer)
  (cycles-to-write-destination-register REGISTER-NOT-USED :type integer)
  (cycles-to-set-flags FLAGS-NOT-SET :type integer)
  (flags-updated nil :type list)
  (source-operand OPERAND-NOT-USED :type risc-operand)
  (destination-operand nil :type risc-operand))

(setq risc-instructions
      (list
	(make-risc-instruction
	 :mnemonic "ABS"
	 :opcode 22
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand OPERAND-NOT-USED
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "ADD"
	 :opcode 0
	 :cycles-to-read-source-register 1
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "ADDC"
	 :opcode 1
	 :cycles-to-read-source-register 1
	 :cycles-to-read-destination-register 3
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "ADDQ"
	 :opcode 2
	 :cycles-to-read-source-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "ADDQMOD"
	 :opcode 63
	 :supported-by-gpu nil
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3 
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "ADDQT"
	 :opcode 3
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
      	(make-risc-instruction
	 :mnemonic "AND"
	 :opcode 9
	 :cycles-to-read-source-register 1
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3 
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "BCLR"
	 :opcode 15
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3 
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "BSET"
	 :opcode 14
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3 
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "BTST"
	 :opcode 13
	 :cycles-to-read-destination-register 1
	 :cycles-to-set-flags 3 
	 :flags-updated '(:cpu-flag-zero)
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "CMP"
	 :opcode 30
	 :cycles-to-read-source-register 1 
	 :cycles-to-read-destination-register 1
	 :cycles-to-set-flags 3 
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "CMPQ"
	 :opcode 31
	 :cycles-to-read-destination-register 1
	 :cycles-to-set-flags 3 
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "DIV"
	 :opcode 21
	 :cycles-to-read-source-register 1 
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 18
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "IMACN"
	 :opcode 20
	 :cycles-to-read-source-register 1 
	 :cycles-to-read-destination-register 1
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "IMULT"
	 :opcode 17
	 :cycles-to-read-source-register 1 
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3 
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "IMULT"
	 :opcode 18
	 :cycles-to-read-source-register 1 
	 :cycles-to-read-destination-register 1
	 :cycles-to-set-flags 3 
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "JR"
	 :opcode 53
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :immediate5))
	(make-risc-instruction
	 :mnemonic "JUMP"
	 :opcode 52
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :indirect))
	(make-risc-instruction
	 :mnemonic "LOAD"
	 :opcode 41
	 :cycles-to-read-source-register 1 
	 :cycles-to-write-destination-register 3 ; special
	 :source-operand (make-risc-operand :kind :indirect)
	 :destination-operand (make-risc-operand :kind :register))	
	(make-risc-instruction
	 :mnemonic "LOAD"
	 :opcode 43
	 :cycles-to-read-source-register 1 
	 :cycles-to-write-destination-register 5 ; special
	 :source-operand (make-risc-operand :kind :r14-indexed)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "LOAD"
	 :opcode 44
	 :cycles-to-read-source-register 1 
	 :cycles-to-write-destination-register 5 ; special
	 :source-operand (make-risc-operand :kind :r15-indexed)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "LOAD"
	 :opcode 58
	 :cycles-to-read-source-register 1 
	 :cycles-to-write-destination-register 5 ; special
	 :source-operand (make-risc-operand :kind :r14-offset)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "LOAD"
	 :opcode 59
	 :cycles-to-read-source-register 1 
	 :cycles-to-write-destination-register 5 ; special
	 :source-operand (make-risc-operand :kind :r15-offset)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "LOADB"
	 :opcode 39
	 :cycles-to-read-source-register 1 
	 :cycles-to-write-destination-register 5 ; check. bus latency
	 :source-operand (make-risc-operand :kind :indirect)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "LOADW"
	 :opcode 40
	 :cycles-to-read-source-register 1 
	 :cycles-to-write-destination-register 5 ; check. bus latency
	 :source-operand (make-risc-operand :kind :indirect)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "LOADP"
	 :opcode 42
	 :supported-by-dsp nil
	 :cycles-to-read-source-register 1 
	 :cycles-to-write-destination-register 5 ; check. bus latency
	 :source-operand (make-risc-operand :kind :indirect)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "MIRROR"
	 :opcode 48
	 :supported-by-gpu nil
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3 
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "MMULT"
	 :opcode 54
	 :cycles-to-read-source-register 1 ; TODO
	 :cycles-to-write-destination-register 32 ; TODO
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "MOVE"
	 :opcode 34
	 :cycles-to-read-source-register 1
	 :cycles-to-write-destination-register 2
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "MOVE"
	 :opcode 51
	 :cycles-to-write-destination-register 2
	 :source-operand (make-risc-operand :kind :pc)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "MOVEFA"
	 :opcode 37
	 :cycles-to-read-source-register 1
	 :cycles-to-write-destination-register 2
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "MOVEI"
	 :opcode 38
	 :cycles-to-write-destination-register 3
	 :source-operand (make-risc-operand :kind :immediate32)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "MOVEQ"
	 :opcode 35
	 :cycles-to-write-destination-register 2
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "MOVETA"
	 :opcode 36
	 :cycles-to-read-source-register 1
	 :cycles-to-write-destination-register 2
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "MTOI"
	 :opcode 55
	 :cycles-to-read-source-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "MULT"
	 :opcode 16
	 :cycles-to-read-destination-register 1
	 :cycles-to-read-source-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "NEG"
	 :opcode 8
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "NOP"
	 :opcode 57
	 :destination-operand OPERAND-NOT-USED)
	(make-risc-instruction
	 :mnemonic "NORMI"
	 :opcode 56
	 :cycles-to-read-source-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "NOT"
	 :opcode 12
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "OR"
	 :opcode 10
	 :cycles-to-read-source-register 1
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "PACK"
	 :opcode 63
	 :supported-by-dsp nil
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "RESMAC"
	 :opcode 19
	 :cycles-to-write-destination-register 3
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "ROR"
	 :opcode 28
	 :cycles-to-read-source-register 1
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "RORQ"
	 :opcode 29
	 :cycles-to-read-source-register 1
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SAT8"
	 :opcode 32
	 :supported-by-dsp nil
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SAT16"
	 :opcode 33
	 :supported-by-dsp nil
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SAT16S"
	 :opcode 33
	 :supported-by-gpu nil
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SAT24"
	 :opcode 62
	 :supported-by-dsp nil
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SAT32S"
	 :opcode 42
	 :supported-by-gpu nil
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-zero :cpu-flag-negative)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SH"
	 :opcode 23
	 :cycles-to-read-source-register 1
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SHA"
	 :opcode 26
	 :cycles-to-read-source-register 1
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SHARQ"
	 :opcode 27
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SHLQ"
	 :opcode 24
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SHRQ"
	 :opcode 25
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "STORE"
	 :opcode 47
	 :cycles-to-read-source-register 1
	 :cycles-to-read-destination-register 1
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :indirect))
	(make-risc-instruction
	 :mnemonic "STORE"
	 :opcode 49
	 :cycles-to-read-source-register 2
	 :cycles-to-write-destination-register 4
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :r14-indexed))
	(make-risc-instruction
	 :mnemonic "STORE"
	 :opcode 50
	 :cycles-to-read-source-register 2
	 :cycles-to-write-destination-register 4
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :r15-indexed))
	(make-risc-instruction
	 :mnemonic "STORE"
	 :opcode 60
	 :cycles-to-read-source-register 2
	 :cycles-to-write-destination-register 4
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :r14-offset))
	(make-risc-instruction
	 :mnemonic "STORE"
	 :opcode 61
	 :cycles-to-read-source-register 2
	 :cycles-to-write-destination-register 4
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :r15-offset))
	(make-risc-instruction
	 :mnemonic "STOREB"
	 :opcode 45
	 :cycles-to-read-source-register 1
	 :cycles-to-write-destination-register 3
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :indirect))
	(make-risc-instruction
	 :mnemonic "STOREW"
	 :opcode 46
	 :cycles-to-read-source-register 1
	 :cycles-to-write-destination-register 3
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :indirect))
	(make-risc-instruction
	 :mnemonic "STOREP"
	 :opcode 45
	 :supported-by-dsp nil
	 :cycles-to-read-source-register 1
	 :cycles-to-write-destination-register 3
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :indirect))
	(make-risc-instruction
	 :mnemonic "SUB"
	 :opcode 4
	 :cycles-to-read-source-register 1
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SUB"
	 :opcode 5
	 :cycles-to-read-source-register 1
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SUBQ"
	 :opcode 4
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SUBQMOD"
	 :opcode 32
	 :supported-by-gpu nil
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "SUBQT"
	 :opcode 7
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :source-operand (make-risc-operand :kind :immediate5)
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "UNPACK"
	 :opcode 63
	 :supported-by-dsp nil
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :destination-operand (make-risc-operand :kind :register))
	(make-risc-instruction
	 :mnemonic "XOR"
	 :opcode 11
	 :cycles-to-read-source-register 1
	 :cycles-to-read-destination-register 1
	 :cycles-to-write-destination-register 3
	 :cycles-to-set-flags 3
	 :flags-updated '(:cpu-flag-carry :cpu-flag-zero :cpu-flag-negative)
	 :source-operand (make-risc-operand :kind :register)
	 :destination-operand (make-risc-operand :kind :register))
	)
      )

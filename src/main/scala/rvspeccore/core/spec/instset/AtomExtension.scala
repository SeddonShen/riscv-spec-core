package rvspeccore.core.spec.instset

import chisel3._
import chisel3.util._

import rvspeccore.core.BaseCore
import rvspeccore.core.spec._
import rvspeccore.core.tool.BitTool._
import rvspeccore.core.tool.LoadStore
import rvspeccore.core.spec.instset.csr._
import os.temp

/** “A” Standard Extension Instructions for Atomic Instructions
  *
  *   - riscv-spec-20191213
  *   - Chapter 24: RV32/64G Instruction Set Listings
  *     - Table 24.2: Instruction listing for RISC-V
  */
trait AExtensionInsts {
  // - RV32A Standard Extension
  val LR_W        = Inst("b00010_?_?_00000_?????_010_?????_0101111")
  val SC_W        = Inst("b00011_?_?_?????_?????_010_?????_0101111")
  val AMOSWAP_W   = Inst("b00001_?_?_?????_?????_010_?????_0101111")
  val AMOADD_W    = Inst("b00000_?_?_?????_?????_010_?????_0101111")
  val AMOXOR_W    = Inst("b00100_?_?_?????_?????_010_?????_0101111")
  val AMOAND_W    = Inst("b01100_?_?_?????_?????_010_?????_0101111")
  val AMOOR_W     = Inst("b01000_?_?_?????_?????_010_?????_0101111")
  val AMOMIN_W    = Inst("b10000_?_?_?????_?????_010_?????_0101111")
  val AMOMAX_W    = Inst("b10100_?_?_?????_?????_010_?????_0101111")
  val AMOMINU_W   = Inst("b11000_?_?_?????_?????_010_?????_0101111")
  val AMOMAXU_W   = Inst("b11100_?_?_?????_?????_010_?????_0101111")

  // - RV64A Standard Extension (in addition to RV32A)
  val LR_D        = Inst("b00010_?_?_00000_?????_011_?????_0101111")
  val SC_D        = Inst("b00011_?_?_?????_?????_011_?????_0101111")
  val AMOSWAP_D   = Inst("b00001_?_?_?????_?????_011_?????_0101111")
  val AMOADD_D    = Inst("b00000_?_?_?????_?????_011_?????_0101111")
  val AMOXOR_D    = Inst("b00100_?_?_?????_?????_011_?????_0101111")
  val AMOAND_D    = Inst("b01100_?_?_?????_?????_011_?????_0101111")
  val AMOOR_D     = Inst("b01000_?_?_?????_?????_011_?????_0101111")
  val AMOMIN_D    = Inst("b10000_?_?_?????_?????_011_?????_0101111")
  val AMOMAX_D    = Inst("b10100_?_?_?????_?????_011_?????_0101111")
  val AMOMINU_D   = Inst("b11000_?_?_?????_?????_011_?????_0101111")
  val AMOMAXU_D   = Inst("b11100_?_?_?????_?????_011_?????_0101111")
  // R_type 
}

// scalafmt: { maxColumn = 200 }

/** “M” Standard Extension for Integer Multiplication and Division
  *
  *   - riscv-spec-20191213
  *   - Chapter 7: “M” Standard Extension for Integer Multiplication and
  *     Division, Version 2.0
  */
trait AExtension extends BaseCore with CommonDecode with AExtensionInsts  with LoadStore{
  // - Table 7.1: Semantics for division by zero and division overflow.
  // : L is the width of the operation in bits:
  // : XLEN for DIV[U] and REM[U], or 32 for DIV[U]W and REM[U]W.
  // def opDIV(divisor: UInt, dividend: UInt, L: Int): UInt = {
  //   MuxCase(
  //     (divisor.asSInt / dividend.asSInt)(L - 1, 0).asUInt, // (L-1, 0) cut extra bit in double sign bit
  //     Array(
  //       (dividend === 0.U(L))                                                        -> -1.S(L.W).asUInt,
  //       (divisor === -(1 << (L - 1)).S(L.W).asUInt && dividend === -1.S(L.W).asUInt) -> -(1 << (L - 1)).S(L.W).asUInt
  //     )
  //   )
  // }
  // TODO: Need to merge with IBase addrAlignedA
  def addrAlignedA(size: UInt, addr: UInt): Bool = {
      MuxLookup(
        size,
        false.B,
        Array(
          "b00".U   -> true.B,              //b
          "b01".U   -> (addr(0)   === 0.U), //h
          "b10".U   -> (addr(1,0) === 0.U), //w
          "b11".U   -> (addr(2,0) === 0.U)  //d
        )
      )
  }

  def doRV32A: Unit = {
    // - temporarily ignore the aqrl bit
    // For RV64, 32-bit AMOs always sign-extend the value placed in rd
    when(LR_W(inst)) { 
      decodeR; 
      // when(addrAlignedA(SizeOp.w, now.reg(rs1))){
      //   next.reg(rd) := signExt(memRead(now.reg(rs1), 32.U)(31, 0), XLEN) 
      // }.otherwise{
      //   mem.read.addr := now.reg(rs1)
      //   raiseException(MExceptionCode.loadAddressMisaligned)
      // }
    }
    when(SC_W(inst)) { 
      decodeR; 
      // when(addrAlignedA(SizeOp.w, now.reg(rs1))){
      //   memWrite(now.reg(rs1), 32.U, now.reg(rs2)(31, 0)) 
      //   now.reg(rd) := 0.U(XLEN.W)
      // }.otherwise{
      //   now.reg(rd) := 1.U(XLEN.W)
      //   mem.write.addr := now.reg(rs1) + imm
      //   raiseException(MExceptionCode.storeOrAMOAddressMisaligned)
      // }
    }
    when(AMOADD_W(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.w, now.reg(rs1))){
        val temp = signExt(memRead(now.reg(rs1), 32.U)(31, 0), XLEN)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 32.U, (temp + now.reg(rs2))(31, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }

    when(AMOAND_W(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.w, now.reg(rs1))){
        val temp = signExt(memRead(now.reg(rs1), 32.U)(31, 0), XLEN)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 32.U, (temp & now.reg(rs2))(31, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }

    when(AMOOR_W(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.w, now.reg(rs1))){
        val temp = signExt(memRead(now.reg(rs1), 32.U)(31, 0), XLEN)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 32.U, (temp | now.reg(rs2))(31, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      }  
    }

    when(AMOXOR_W(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.w, now.reg(rs1))){
        val temp = signExt(memRead(now.reg(rs1), 32.U)(31, 0), XLEN)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 32.U, (temp ^ now.reg(rs2))(31, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }
    when(AMOSWAP_W(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.w, now.reg(rs1))){
        val temp = signExt(memRead(now.reg(rs1), 32.U)(31, 0), XLEN)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 32.U, now.reg(rs2)(31, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }
    when(AMOMAX_W(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.w, now.reg(rs1))){
        val temp = signExt(memRead(now.reg(rs1), 32.U)(31, 0), XLEN)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 32.U, Mux(now.reg(rs2).asSInt > temp.asSInt, now.reg(rs2), temp)(31, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }
    when(AMOMAXU_W(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.w, now.reg(rs1))){
        val temp = signExt(memRead(now.reg(rs1), 32.U)(31, 0), XLEN)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 32.U, Mux(now.reg(rs2) > temp, now.reg(rs2), temp)(31, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }
    when(AMOMIN_W(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.w, now.reg(rs1))){
        val temp = signExt(memRead(now.reg(rs1), 32.U)(31, 0), XLEN)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 32.U, Mux(now.reg(rs2).asSInt < temp.asSInt, now.reg(rs2), temp)(31, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }
    when(AMOMINU_W(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.w, now.reg(rs1))){
        val temp = signExt(memRead(now.reg(rs1), 32.U)(31, 0), XLEN)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 32.U, Mux(now.reg(rs2) < temp, now.reg(rs2), temp)(31, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }
  }

  def doRV64A: Unit = {
    doRV32A
    when(LR_D(inst)) { 
      decodeR; 
      // when(addrAlignedA(SizeOp.d, now.reg(rs1))){
      //   next.reg(rd) := signExt(memRead(now.reg(rs1), 64.U)(63, 0), XLEN) 
      // }.otherwise{
      //   mem.read.addr := now.reg(rs1)
      //   raiseException(MExceptionCode.loadAddressMisaligned)
      // }
    }
    when(SC_D(inst)) { 
      decodeR; 
      // when(addrAlignedA(SizeOp.d, now.reg(rs1))){
      //   memWrite(now.reg(rs1), 64.U, now.reg(rs2)(63, 0)) 
      //   now.reg(rd) := 0.U(XLEN.W)
      // }.otherwise{
      //   now.reg(rd) := 1.U(XLEN.W)
      //   mem.write.addr := now.reg(rs1) + imm
      //   raiseException(MExceptionCode.storeOrAMOAddressMisaligned)
      // }
    }
    when(AMOADD_D(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.d, now.reg(rs1))){
        val temp = memRead(now.reg(rs1), 64.U)(63, 0)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 64.U, (temp + now.reg(rs2))(63, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }

    when(AMOAND_D(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.d, now.reg(rs1))){
        val temp = memRead(now.reg(rs1), 64.U)(63, 0)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 64.U, (temp & now.reg(rs2))(63, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      }  
    }

    when(AMOOR_D(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.d, now.reg(rs1))){
        val temp = memRead(now.reg(rs1), 64.U)(63, 0)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 64.U, ( temp | now.reg(rs2))(63, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      }  
    }

    when(AMOXOR_D(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.d, now.reg(rs1))){
        val temp = memRead(now.reg(rs1), 64.U)(63, 0)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 64.U, ( temp ^ now.reg(rs2))(63, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }
    when(AMOSWAP_D(inst)){
      decodeR; 
      printf("[Debug]AMOSWAP_D rs1: %x rs2: %x\n", now.reg(rs1), now.reg(rs2))
      when(addrAlignedA(SizeOp.d, now.reg(rs1))){
        val temp = memRead(now.reg(rs1), 64.U)(63, 0)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 64.U, ( now.reg(rs2))(63, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }
    when(AMOMAX_D(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.d, now.reg(rs1))){
        val temp = memRead(now.reg(rs1), 64.U)(63, 0)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 64.U, Mux(now.reg(rs2).asSInt > temp.asSInt, now.reg(rs2), temp)(63, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }
    when(AMOMAXU_D(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.d, now.reg(rs1))){
        val temp = memRead(now.reg(rs1), 64.U)(63, 0)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 64.U, Mux(now.reg(rs2) > temp, now.reg(rs2), temp)(63, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }
    when(AMOMIN_D(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.d, now.reg(rs1))){
        val temp = memRead(now.reg(rs1), 64.U)(63, 0)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 64.U, Mux(now.reg(rs2).asSInt < temp.asSInt, now.reg(rs2), temp)(63, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }
    when(AMOMINU_D(inst)){
      decodeR; 
      when(addrAlignedA(SizeOp.d, now.reg(rs1))){
        val temp = memRead(now.reg(rs1), 64.U)(63, 0)
        next.reg(rd) := temp
        memWrite(now.reg(rs1), 64.U, Mux(now.reg(rs2) < temp, now.reg(rs2), temp)(63, 0) ) 
      }.otherwise{
        mem.read.addr := now.reg(rs1)
        raiseException(MExceptionCode.loadAddressMisaligned)
      } 
    }
  }
}

// scalafmt: { maxColumn = 120 } (back to defaults)

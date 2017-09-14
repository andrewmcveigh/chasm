module Elf

import Data.Bits
import Data.Bits as Bit
import Data.Buffer

%access public export

bs : Integer -> Integer -> List Bits8
bs 8 = reverse . b64ToBytes . fromInteger
bs 4 = reverse . b32ToBytes . fromInteger
bs 2 = reverse . b16ToBytes . fromInteger
bs 1 = reverse . b8ToBytes  . fromInteger
bs _ = assert_unreachable

PT_LOAD : Integer; PT_LOAD = 0x01
PF_X    : Integer; PF_X    = 0x01
PF_W    : Integer; PF_W    = 0x02
PF_R    : Integer; PF_R    = 0x04

EI_MAGIC   : List Bits8; EI_MAGIC   = [0x7f, 0x45, 0x4c, 0x46]
EI_CLASS   : List Bits8; EI_CLASS   = [2]
EI_DATA    : List Bits8; EI_DATA    = [1]
EI_VERSION : List Bits8; EI_VERSION = [1]
EI_OSABI   : List Bits8; EI_OSABI   = [1]
EI_ABIVER  : List Bits8; EI_ABIVER  = [0]
EI_PAD     : List Bits8; EI_PAD     = [0, 0, 0, 0, 0, 0, 0]

E_TYPE    : Integer; E_TYPE    = 0x02 -- executable
E_MACHINE : Integer; E_MACHINE = 0x3e -- x86-64
E_VERSION : Integer; E_VERSION = 0x01 -- original
E_ENTRY   : Integer; E_ENTRY   = 0x4000e8
E_PHOFF   : Integer; E_PHOFF   = 0x40 -- 64 bit arch = 64 bytes
E_FLAGS   : Integer; E_FLAGS   = 0x00
E_EHSIZE  : Integer; E_EHSIZE  = 0x40 -- 64 bit arch = 64 bytes

interface ToBytes a where
  toBytes : a -> List Bits8

record EIdent where
  constructor MkEIdent
  ei_magic   : List Bits8
  ei_class   : List Bits8
  ei_data    : List Bits8
  ei_version : List Bits8
  ei_osabi   : List Bits8
  ei_abiver  : List Bits8
  ei_pad     : List Bits8

implementation ToBytes EIdent where
  toBytes (MkEIdent a b c d e f g) = a ++ b ++ c ++ d ++ e ++ f ++ g

E_IDENT : EIdent
E_IDENT =
  MkEIdent EI_MAGIC EI_CLASS EI_DATA EI_VERSION EI_OSABI EI_ABIVER EI_PAD

record FileHeader where
  constructor MkFileHeader
  e_ident     : EIdent
  e_phentsize : Integer
  e_phnum     : Integer
  e_shentsize : Integer
  e_shnum     : Integer
  e_shstrndx  : Integer
  e_shoff     : Integer

implementation ToBytes FileHeader where
  toBytes hdr =
    (toBytes $ e_ident hdr) ++
    (bs 2 E_TYPE) ++
    (bs 2 E_MACHINE) ++
    (bs 4 E_VERSION) ++
    (bs 8 E_ENTRY) ++
    (bs 8 E_PHOFF) ++
    (bs 8 $ e_shoff hdr) ++
    (bs 4 E_FLAGS) ++
    (bs 2 E_EHSIZE) ++
    (bs 2 $ e_phentsize hdr) ++
    (bs 2 $ e_phnum hdr) ++
    (bs 2 $ e_shentsize hdr) ++
    (bs 2 $ e_shnum hdr) ++
    (bs 2 $ e_shstrndx hdr)

record ProgramHeaderEntry where
  constructor MkProgramHeaderEntry
  type   : Integer
  flags  : Integer
  offset : Integer
  vaddr  : Integer
  paddr  : Integer
  filesz : Integer
  memsz  : Integer
  align  : Integer

ProgramHeader : Type
ProgramHeader = List ProgramHeaderEntry

implementation ToBytes ProgramHeaderEntry where
  toBytes hdr =
    (bs 4 $ type   hdr) ++
    (bs 4 $ flags  hdr) ++
    (bs 8 $ offset hdr) ++
    (bs 8 $ vaddr  hdr) ++
    (bs 8 $ paddr  hdr) ++
    (bs 8 $ filesz hdr) ++
    (bs 8 $ memsz  hdr) ++
    (bs 8 $ align  hdr)

record Program where
  constructor MkProgram
  header : List ProgramHeaderEntry
  bytes  : List Bits8
  size   : Integer

implementation ToBytes Program where
  toBytes (MkProgram hdr bytes size) =
    (concat $ map toBytes hdr) ++ bytes

fileHeader : Program -> FileHeader
fileHeader (MkProgram [] _ _)          = MkFileHeader E_IDENT 0 0 0 0 0 0
fileHeader (MkProgram (ph :: phs) _ _) =
  let ePhentSize = cast $ length $ toBytes ph
      ePhNum     = cast $ length phs + 1
      eShNum     = 0
  in MkFileHeader E_IDENT ePhentSize ePhNum 0 eShNum 0 0

b8sToBuffer : List Bits8 -> IO (Maybe Buffer)
b8sToBuffer b8s =
  do buf <- newBuffer $ cast $ length b8s
     case buf of
       Just buf => do _ <- writeBytes buf 0 b8s
                      pure $ Just buf
       Nothing  => pure Nothing
  where writeBytes : Buffer -> Int -> List Bits8 -> IO ()
        writeBytes buf _ []        = pure ()
        writeBytes buf n (b :: bs) = do _ <- setByte buf n b
                                        writeBytes buf (n + 1) bs

writeFile : String -> List Bits8 -> IO (Either FileError ())
writeFile fn b8s =
  do Right h  <- openFile fn WriteTruncate | Left err => pure (Left err)
     Just buf <- b8sToBuffer b8s           | Nothing  => pure (Left FileWriteError)
     _        <- writeBufferToFile h buf $ cast $ length b8s
     pure $ Right ()

program : List Bits8 -> List Bits8 -> Program
program instructions prgdata =
  let ilen   = length instructions
      iSize  = E_EHSIZE + (3 * 56) + cast ilen
      dSize  = cast $ length prgdata
      memOff = 0x600000 + iSize
      hdr1   = MkProgramHeaderEntry PT_LOAD (PF_X + PF_R) 0x0000 0x400000 0x400000 iSize iSize 0x200000
      hdr2   = MkProgramHeaderEntry PT_LOAD (PF_W + PF_R) iSize memOff memOff dSize dSize 0x200000
      -- 0x010a <- is wrong, should be dynamic
      hdr3   = MkProgramHeaderEntry 0x65041580 0x2800 0 0 0 0x08 0 0
      prog   = instructions ++ prgdata
  in MkProgram [hdr1, hdr2, hdr3] prog $ cast $ length prog

writeProgram : String -> Program -> IO (Either FileError ())
writeProgram fn prog =
  writeFile fn $ (toBytes $ fileHeader prog) ++ toBytes prog

programInstructions : List Bits8
programInstructions =
  [0xba,  0x0e, 0x00, 0x00, 0x00,
   0xb9,  0x0a, 0x01, 0x60, 0x00,
   0xbb,  0x01, 0x00, 0x00, 0x00,
   0xb8,  0x04, 0x00, 0x00, 0x00,
   0xcd,  0x80,
   0xbb,  0x00, 0x00, 0x00, 0x00,
   0xb8,  0x01, 0x00, 0x00, 0x00,
   0xcd,  0x80
  ]

fromChar : Char -> Bits8
fromChar c = fromInteger $ cast $ ord c

helloWorld : List Bits8
helloWorld = map fromChar $ unpack "Hello, World!\n"

prog : Program
prog = program programInstructions helloWorld

main : IO ()
main = do _ <- writeProgram "/tmp/hello" prog
          pure ()

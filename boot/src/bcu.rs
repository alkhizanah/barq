//! Barq Compilation Unit
//!
//! A data structure containing the resources that gets shared between pipeline

use std::str::FromStr;

use crate::{
    ast::Ast,
    bir::Bir,
    cfg_match,
    lowerer::{Lowerer, LowererResult},
    parser::{Parser, ParserResult},
};

pub struct Bcu {
    pub global_assembly: String,
    pub target: Target,
}

impl Bcu {
    pub const fn new(target: Target) -> Bcu {
        Bcu {
            global_assembly: String::new(),
            target,
        }
    }

    pub fn parse(&mut self, file: &SourceFile) -> ParserResult<Ast> {
        Parser::new(self, file).parse()
    }

    pub fn lower(&self, file: &SourceFile, ast: Ast) -> LowererResult<Bir> {
        Lowerer::new(self, file, ast).lower()
    }
}

pub struct SourceFile {
    pub path: String,
    pub buffer: String,
}

impl SourceFile {
    pub const fn new(path: String, buffer: String) -> SourceFile {
        SourceFile { path, buffer }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Target {
    pub cpu: Cpu,
    pub os: Os,
    pub abi: Abi,
    pub of: ObjectFormat,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum CType {
    Char,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
    Float,
    Double,
    LongDouble,
}

impl Target {
    pub fn native() -> Target {
        let arch = cfg_match! {{
            target_arch = "amdgpu" => { Arch::AmdGcn }
            target_arch = "arm" => { Arch::Arm }
            all(target_arch = "aarch64", target_endian = "big") => { Arch::Aarch64be }
            all(target_arch = "aarch64", target_endian = "little") => { Arch::Aarch64 }
            target_arch = "avr" => { Arch::Avr }
            target_arch = "csky" => { Arch::Csky }
            target_arch = "hexagon" => { Arch::Hexagon }
            target_arch = "loongarch64" => { Arch::LoongArch64 }
            target_arch = "m68k" => { Arch::M68k }
            all(target_arch = "mips", target_endian = "big") => { Arch::Mips }
            all(target_arch = "mips", target_endian = "little") => { Arch::Mipsel }
            all(target_arch = "mips64", target_endian = "big") => { Arch::Mips64 }
            all(target_arch = "mips64", target_endian = "little") => { Arch::Mips64el }
            target_arch = "msp430" => { Arch::Msp430 }
            target_arch = "nvptx64" => { Arch::Nvptx64 }
            all(target_arch = "powerpc", target_endian = "big") => { Arch::PowerPc }
            all(target_arch = "powerpc", target_endian = "little") => { Arch::PowerPcle }
            all(target_arch = "powerpc64", target_endian = "big") => { Arch::PowerPc64 }
            all(target_arch = "powerpc64", target_endian = "little") => { Arch::PowerPc64le }
            target_arch = "riscv32" => { Arch::Riscv32 }
            target_arch = "riscv64" => { Arch::Riscv64 }
            target_arch = "s390x" => { Arch::S390x }
            target_arch = "sparc" => { Arch::Sparc }
            target_arch = "sparc64" => { Arch::Sparc64 }
            target_arch = "wasm32" => { Arch::Wasm32 }
            target_arch = "wasm64" => { Arch::Wasm64 }
            target_arch = "x86" => { Arch::X86 }
            target_arch = "x86_64" => { Arch::X86_64 }
            target_arch = "xcore" => { Arch::Xcore }
            target_arch = "xtensa" => { Arch::Xtensa }
        }};

        let os = cfg_match! {{
            target_os = "none" => { Os::Freestanding }
            target_os = "unknown" => { Os::Other }
            target_os = "fuchsia" => { Os::Fuchsia }
            target_os = "hermit" => { Os::Hermit }
            target_os = "aix" => { Os::Aix }
            target_os = "haiku" => { Os::Haiku }
            target_os = "hurd" => { Os::Hurd }
            target_os = "linux" => { Os::Linux }
            target_os = "android" => { Os::Linux }
            target_os = "plan9" => { Os::Plan9 }
            target_os = "rtems" => { Os::Rtems }
            target_os = "serenity" => { Os::Serenity }
            target_os = "zos" => { Os::Zos }
            target_os = "dragonfly" => { Os::Dragonfly }
            target_os = "freebsd" => { Os::FreeBsd }
            target_os = "netbsd" => { Os::NetBsd }
            target_os = "openbsd" => { Os::OpenBsd }
            target_os = "driverkit" => { Os::Driverkit }
            target_os = "ios" => { Os::Ios }
            target_os = "macos" => { Os::MacOs }
            target_os = "tvos" => { Os::TvOs }
            target_os = "visionos" => { Os::VisionOs }
            target_os = "watchos" => { Os::WatchOs }
            target_os = "illumos" => { Os::Illumos }
            target_os = "solaris" => { Os::Solaris }
            target_os = "windows" => { Os::Windows }
            target_os = "uefi" => { Os::Uefi }
            target_os = "ps3" => { Os::Ps3 }
            target_os = "ps4" => { Os::Ps4 }
            target_os = "ps5" => { Os::Ps5 }
            target_os = "emscripten" => { Os::Emscripten }
            target_os = "wasi" => { Os::Wasi }
            target_os = "amdhsa" => { Os::Amdhsa }
            target_os = "amdpal" => { Os::Amdpal }
            target_os = "cuda" => { Os::Cuda }
            target_os = "mesa3d" => { Os::Mesa3d }
            target_os = "nvcl" => { Os::Nvcl }
            target_os = "opencl" => { Os::Opencl }
            target_os = "opengl" => { Os::Opengl }
            target_os = "vulkan" => { Os::Vulkan }
        }};

        let abi = if cfg!(target_os = "android") {
            Abi::Android
        } else {
            Abi::default(arch, os)
        };

        let of = ObjectFormat::default(arch, os);

        Target {
            cpu: Cpu { arch },
            os,
            abi,
            of,
        }
    }

    pub fn pointer_bit_width(&self) -> u16 {
        match self.abi {
            | Abi::GnuX32 | Abi::MuslX32 | Abi::GnuAbiN32 | Abi::MuslAbiN32 | Abi::Gnuilp32 | Abi::Ilp32 => {
                32
            }

            | Abi::GnuAbi64 | Abi::MuslAbi64 => 64,

            | _ => match self.cpu.arch {
                | Arch::Avr | Arch::Msp430 => 16,

                | Arch::Arc
                | Arch::Arm
                | Arch::Armeb
                | Arch::Csky
                | Arch::Hexagon
                | Arch::M68k
                | Arch::Mips
                | Arch::Mipsel
                | Arch::PowerPc
                | Arch::PowerPcle
                | Arch::Riscv32
                | Arch::Thumb
                | Arch::Thumbeb
                | Arch::X86
                | Arch::Xcore
                | Arch::Nvptx
                | Arch::Kalimba
                | Arch::Lanai
                | Arch::Wasm32
                | Arch::Sparc
                | Arch::Spirv32
                | Arch::LoongArch32
                | Arch::Xtensa
                | Arch::Propeller => 32,

                | Arch::Aarch64
                | Arch::Aarch64be
                | Arch::Mips64
                | Arch::Mips64el
                | Arch::PowerPc64
                | Arch::PowerPc64le
                | Arch::Riscv64
                | Arch::X86_64
                | Arch::Nvptx64
                | Arch::Wasm64
                | Arch::AmdGcn
                | Arch::Bpfel
                | Arch::Bpfeb
                | Arch::Sparc64
                | Arch::S390x
                | Arch::Ve
                | Arch::Spirv
                | Arch::Spirv64
                | Arch::LoongArch64 => 64,
            },
        }
    }

    pub fn c_type_bit_width(&self, c_type: CType) -> u16 {
        match self.os {
            | Os::Freestanding | Os::Other => match self.cpu.arch {
                | Arch::Msp430 => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort | CType::Int | CType::UInt => 16,
                    | CType::Float | CType::Long | CType::ULong => 32,
                    | CType::LongLong | CType::ULongLong | CType::Double | CType::LongDouble => 64,
                },

                | Arch::Avr => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort | CType::Int | CType::UInt => 16,
                    | CType::Long | CType::ULong | CType::Float | CType::Double | CType::LongDouble => 32,
                    | CType::LongLong | CType::ULongLong => 64,
                },

                | Arch::Mips64 | Arch::Mips64el => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort => 16,
                    | CType::Int | CType::UInt | CType::Float => 32,

                    | CType::Long | CType::ULong => match self.abi {
                        | Abi::GnuAbiN32 | Abi::MuslAbiN32 => 32,
                        | _ => 64,
                    },

                    | CType::LongLong | CType::ULongLong | CType::Double => 64,
                    | CType::LongDouble => 128,
                },

                | Arch::X86_64 => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort => 16,
                    | CType::Int | CType::UInt | CType::Float => 32,

                    | CType::Long | CType::ULong => match self.abi {
                        | Abi::GnuX32 | Abi::MuslX32 => 32,
                        | _ => 64,
                    },

                    | CType::LongLong | CType::ULongLong | CType::Double => 64,
                    | CType::LongDouble => 80,
                },

                | _ => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort => 16,
                    | CType::Int | CType::UInt | CType::Float => 32,
                    | CType::Long | CType::ULong => self.pointer_bit_width(),
                    | CType::LongLong | CType::ULongLong | CType::Double => 64,

                    | CType::LongDouble => match self.cpu.arch {
                        | Arch::X86 => match self.abi {
                            | Abi::Android => 64,
                            | _ => 80,
                        },

                        | Arch::PowerPc | Arch::PowerPcle | Arch::PowerPc64 | Arch::PowerPc64le => {
                            match self.abi {
                                | Abi::Musl
                                | Abi::MuslAbiN32
                                | Abi::MuslAbi64
                                | Abi::MuslEAbi
                                | Abi::MuslEAbiHf
                                | Abi::MuslX32 => 64,
                                | _ => 128,
                            }
                        }

                        | Arch::Riscv32
                        | Arch::Riscv64
                        | Arch::Aarch64
                        | Arch::Aarch64be
                        | Arch::S390x
                        | Arch::Sparc64
                        | Arch::Wasm32
                        | Arch::Wasm64
                        | Arch::LoongArch32
                        | Arch::LoongArch64
                        | Arch::Ve => 128,

                        | _ => 64,
                    },
                },
            },

            | Os::Elfiamcu
            | Os::Fuchsia
            | Os::Hermit
            | Os::Aix
            | Os::Haiku
            | Os::Hurd
            | Os::Linux
            | Os::Plan9
            | Os::Rtems
            | Os::Serenity
            | Os::Zos
            | Os::FreeBsd
            | Os::Dragonfly
            | Os::NetBsd
            | Os::OpenBsd
            | Os::Illumos
            | Os::Solaris
            | Os::Wasi
            | Os::Emscripten => match self.cpu.arch {
                | Arch::Msp430 => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort | CType::Int | CType::UInt => 16,
                    | CType::Long | CType::ULong | CType::Float => 32,
                    | CType::LongLong | CType::ULongLong | CType::Double | CType::LongDouble => 64,
                },

                | Arch::Avr => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort | CType::Int | CType::UInt => 16,
                    | CType::Long | CType::ULong | CType::Float | CType::Double | CType::LongDouble => 32,
                    | CType::LongLong | CType::ULongLong => 64,
                },

                | Arch::Mips64 | Arch::Mips64el => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort => 16,
                    | CType::Int | CType::UInt | CType::Float => 32,

                    | CType::Long | CType::ULong => match self.abi {
                        | Abi::GnuAbiN32 | Abi::MuslAbiN32 => 32,
                        | _ => 64,
                    },

                    | CType::LongLong | CType::ULongLong | CType::Double => 64,

                    | CType::LongDouble => {
                        if self.os == Os::FreeBsd {
                            64
                        } else {
                            128
                        }
                    }
                },

                | Arch::X86_64 => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort => 16,
                    | CType::Int | CType::UInt | CType::Float => 32,

                    | CType::Long | CType::ULong => match self.abi {
                        | Abi::GnuX32 | Abi::MuslX32 => 32,
                        | _ => 64,
                    },

                    | CType::LongLong | CType::ULongLong | CType::Double => 64,
                    | CType::LongDouble => 80,
                },

                | _ => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort => 16,
                    | CType::Int | CType::UInt | CType::Float => 32,
                    | CType::Long | CType::ULong => self.pointer_bit_width(),
                    | CType::LongLong | CType::ULongLong | CType::Double => 64,

                    | CType::LongDouble => match self.cpu.arch {
                        | Arch::X86 => match self.abi {
                            | Abi::Android => 64,
                            | _ => match self.os {
                                | Os::Elfiamcu => 64,

                                | _ => 80,
                            },
                        },

                        | Arch::PowerPc | Arch::PowerPcle => match self.abi {
                            | Abi::Musl
                            | Abi::MuslAbiN32
                            | Abi::MuslAbi64
                            | Abi::MuslEAbi
                            | Abi::MuslEAbiHf
                            | Abi::MuslX32 => 64,

                            | _ => match self.os {
                                | Os::Aix | Os::FreeBsd | Os::NetBsd | Os::OpenBsd => 64,
                                | _ => 128,
                            },
                        },

                        | Arch::PowerPc64 | Arch::PowerPc64le => match self.abi {
                            | Abi::Musl
                            | Abi::MuslAbiN32
                            | Abi::MuslAbi64
                            | Abi::MuslEAbi
                            | Abi::MuslEAbiHf
                            | Abi::MuslX32 => 64,

                            | _ => match self.os {
                                | Os::Aix | Os::FreeBsd | Os::OpenBsd => 64,
                                | _ => 128,
                            },
                        },

                        | Arch::Riscv32
                        | Arch::Riscv64
                        | Arch::Aarch64
                        | Arch::Aarch64be
                        | Arch::S390x
                        | Arch::Mips64
                        | Arch::Mips64el
                        | Arch::Sparc64
                        | Arch::Wasm32
                        | Arch::Wasm64
                        | Arch::LoongArch32
                        | Arch::LoongArch64
                        | Arch::Ve => 128,

                        | _ => 64,
                    },
                },
            },

            | Os::Windows | Os::Uefi => match self.cpu.arch {
                | Arch::X86 => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort => 16,
                    | CType::Int | CType::UInt | CType::Float => 32,
                    | CType::Long | CType::ULong => 32,
                    | CType::LongLong | CType::ULongLong | CType::Double => 64,

                    | CType::LongDouble => match self.abi {
                        | Abi::Gnu | Abi::Gnuilp32 | Abi::Ilp32 | Abi::Cygnus => 80,
                        | _ => 64,
                    },
                },

                | Arch::X86_64 => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort => 16,
                    | CType::Int | CType::UInt | CType::Float => 32,

                    | CType::Long | CType::ULong => match self.abi {
                        | Abi::Cygnus => 64,
                        | _ => 32,
                    },

                    | CType::LongLong | CType::ULongLong | CType::Double => 64,

                    | CType::LongDouble => match self.abi {
                        | Abi::Gnu | Abi::Gnuilp32 | Abi::Ilp32 | Abi::Cygnus => 80,
                        | _ => 64,
                    },
                },

                | _ => match c_type {
                    | CType::Char => 8,
                    | CType::Short | CType::UShort => 16,
                    | CType::Int | CType::UInt | CType::Float => 32,
                    | CType::Long | CType::ULong => 32,
                    | CType::LongLong | CType::ULongLong | CType::Double => 64,
                    | CType::LongDouble => 64,
                },
            },

            | Os::Driverkit | Os::Ios | Os::MacOs | Os::TvOs | Os::VisionOs | Os::WatchOs => match c_type {
                | CType::Char => 8,
                | CType::Short | CType::UShort => 16,
                | CType::Int | CType::UInt | CType::Float => 32,

                | CType::Long | CType::ULong => match self.cpu.arch {
                    | Arch::X86_64 => 64,

                    | _ => match self.abi {
                        | Abi::Ilp32 => 32,

                        | _ => 64,
                    },
                },

                | CType::LongLong | CType::ULongLong | CType::Double => 64,

                | CType::LongDouble => match self.cpu.arch {
                    | Arch::X86_64 => 80,

                    | _ => 64,
                },
            },

            | Os::Nvcl | Os::Cuda => match c_type {
                | CType::Char => 8,
                | CType::Short | CType::UShort => 16,
                | CType::Int | CType::UInt | CType::Float => 32,

                | CType::Long | CType::ULong => match self.cpu.arch {
                    | Arch::Nvptx => 32,
                    | Arch::Nvptx64 => 64,

                    | _ => 64,
                },

                | CType::LongLong | CType::ULongLong | CType::Double => 64,
                | CType::LongDouble => 64,
            },

            | Os::Amdhsa | Os::Amdpal | Os::Mesa3d => match c_type {
                | CType::Char => 8,
                | CType::Short | CType::UShort => 16,
                | CType::Int | CType::UInt | CType::Float => 32,
                | CType::Long | CType::ULong | CType::LongLong | CType::ULongLong | CType::Double => 64,
                | CType::LongDouble => 128,
            },

            | Os::Opencl | Os::Vulkan => match c_type {
                | CType::Char => 8,
                | CType::Short | CType::UShort => 16,
                | CType::Int | CType::UInt | CType::Float => 32,
                | CType::Long | CType::ULong | CType::Double => 64,
                | CType::LongLong | CType::ULongLong => 128,
                // Note: The OpenCL specification does not guarantee a particular size for long double,
                // but clang uses 128 bits.
                | CType::LongDouble => 128,
            },

            | Os::Ps4 | Os::Ps5 => match c_type {
                | CType::Char => 8,
                | CType::Short | CType::UShort => 16,
                | CType::Int | CType::UInt | CType::Float => 32,
                | CType::Long | CType::ULong => 64,
                | CType::LongLong | CType::ULongLong | CType::Double => 64,
                | CType::LongDouble => 80,
            },

            | Os::Contiki | Os::Ps3 | Os::Opengl => todo!(),
        }
    }

    pub fn is_c_char_signed(&self) -> bool {
        if self.os.is_darwin() || self.os == Os::Windows || self.os == Os::Uefi {
            return true;
        }

        match self.cpu.arch {
            | Arch::Arm
            | Arch::Armeb
            | Arch::Thumb
            | Arch::Thumbeb
            | Arch::Aarch64
            | Arch::Aarch64be
            | Arch::Arc
            | Arch::Csky
            | Arch::Hexagon
            | Arch::Msp430
            | Arch::PowerPc
            | Arch::PowerPcle
            | Arch::PowerPc64
            | Arch::PowerPc64le
            | Arch::S390x
            | Arch::Riscv32
            | Arch::Riscv64
            | Arch::Xcore
            | Arch::Xtensa => false,

            | _ => true,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TargetFromStrErr {
    MissingArchitecture,
    UnknownArchitecture,
    MissingOperatingSystem,
    UnknownOperatingSystem,
    UnknownApplicationBinaryInterface,
    UnexpectedExtraField,
}

impl ToString for TargetFromStrErr {
    fn to_string(&self) -> String {
        match self {
            | TargetFromStrErr::MissingArchitecture => "architecture not provided in target query",
            | TargetFromStrErr::UnknownArchitecture => "unknown architecture in target query",
            | TargetFromStrErr::MissingOperatingSystem => "operating system not provided in target query",
            | TargetFromStrErr::UnknownOperatingSystem => "unknown operating system in target query",

            | TargetFromStrErr::UnknownApplicationBinaryInterface => {
                "unknown application binary interface in target query"
            }

            | TargetFromStrErr::UnexpectedExtraField => {
                "did not expect fields more than arch-os-abi in target query"
            }
        }
        .to_string()
    }
}

impl FromStr for Target {
    type Err = TargetFromStrErr;

    fn from_str(arch_os_abi: &str) -> Result<Self, Self::Err> {
        let mut arch_os_abi_iter = arch_os_abi.split('-');

        let Some(arch) = arch_os_abi_iter.next() else {
            return Err(TargetFromStrErr::MissingArchitecture);
        };

        let Ok(arch) = Arch::from_str(arch) else {
            return Err(TargetFromStrErr::UnknownArchitecture);
        };

        let cpu = Cpu { arch };

        let Some(os) = arch_os_abi_iter.next() else {
            return Err(TargetFromStrErr::MissingOperatingSystem);
        };

        let Ok(os) = Os::from_str(os) else {
            return Err(TargetFromStrErr::UnknownOperatingSystem);
        };

        let of = ObjectFormat::default(arch, os);

        let abi = if let Some(abi) = arch_os_abi_iter.next() {
            let Ok(abi) = Abi::from_str(abi) else {
                return Err(TargetFromStrErr::UnknownApplicationBinaryInterface);
            };

            if arch_os_abi_iter.next().is_some() {
                return Err(TargetFromStrErr::UnexpectedExtraField);
            }

            abi
        } else {
            Abi::default(arch, os)
        };

        Ok(Target { cpu, os, abi, of })
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Os {
    Freestanding,
    Other,

    Contiki,
    Elfiamcu,
    Fuchsia,
    Hermit,

    Aix,
    Haiku,
    Hurd,
    Linux,
    Plan9,
    Rtems,
    Serenity,
    Zos,

    Dragonfly,
    FreeBsd,
    NetBsd,
    OpenBsd,

    Driverkit,
    Ios,
    MacOs,
    TvOs,
    VisionOs,
    WatchOs,

    Illumos,
    Solaris,

    Windows,
    Uefi,

    Ps3,
    Ps4,
    Ps5,

    Emscripten,
    Wasi,

    Amdhsa,
    Amdpal,
    Cuda,
    Mesa3d,
    Nvcl,
    Opencl,
    Opengl,
    Vulkan,
    // LLVM tags deliberately omitted:
    // - bridgeos
    // - darwin
    // - kfreebsd
    // - nacl
    // - shadermodel
}

impl FromStr for Os {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            | "freestanding" => Os::Freestanding,
            | "other" => Os::Other,

            | "contiki" => Os::Contiki,
            | "elfiamcu" => Os::Elfiamcu,
            | "fuchsia" => Os::Fuchsia,
            | "hermit" => Os::Hermit,

            | "aix" => Os::Aix,
            | "haiku" => Os::Haiku,
            | "hurd" => Os::Hurd,
            | "linux" => Os::Linux,
            | "plan9" => Os::Plan9,
            | "rtems" => Os::Rtems,
            | "serenity" => Os::Serenity,
            | "zos" => Os::Zos,

            | "dragonfly" => Os::Dragonfly,
            | "freebsd" => Os::FreeBsd,
            | "netbsd" => Os::NetBsd,
            | "openbsd" => Os::OpenBsd,

            | "driverkit" => Os::Driverkit,
            | "ios" => Os::Ios,
            | "macos" => Os::MacOs,
            | "tvos" => Os::TvOs,
            | "visionos" => Os::VisionOs,
            | "watchos" => Os::WatchOs,

            | "illumos" => Os::Illumos,
            | "solaris" => Os::Solaris,

            | "windows" => Os::Windows,
            | "uefi" => Os::Uefi,

            | "ps3" => Os::Ps3,
            | "ps4" => Os::Ps4,
            | "ps5" => Os::Ps5,

            | "emscripten" => Os::Emscripten,
            | "wasi" => Os::Wasi,

            | "amdhsa" => Os::Amdhsa,
            | "amdpal" => Os::Amdpal,
            | "cuda" => Os::Cuda,
            | "mesa3d" => Os::Mesa3d,
            | "nvcl" => Os::Nvcl,
            | "opencl" => Os::Opencl,
            | "opengl" => Os::Opengl,
            | "vulkan" => Os::Vulkan,

            | _ => return Err(()),
        })
    }
}

impl Os {
    pub fn is_darwin(&self) -> bool {
        matches!(
            self,
            Os::Driverkit | Os::Ios | Os::MacOs | Os::TvOs | Os::VisionOs | Os::WatchOs
        )
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Cpu {
    pub arch: Arch,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Arch {
    AmdGcn,
    Arc,
    Arm,
    Armeb,
    Thumb,
    Thumbeb,
    Aarch64,
    Aarch64be,
    Avr,
    Bpfel,
    Bpfeb,
    Csky,
    Hexagon,
    Kalimba,
    Lanai,
    LoongArch32,
    LoongArch64,
    M68k,
    Mips,
    Mipsel,
    Mips64,
    Mips64el,
    Msp430,
    Nvptx,
    Nvptx64,
    PowerPc,
    PowerPcle,
    PowerPc64,
    PowerPc64le,
    Propeller,
    Riscv32,
    Riscv64,
    S390x,
    Sparc,
    Sparc64,
    Spirv,
    Spirv32,
    Spirv64,
    Ve,
    Wasm32,
    Wasm64,
    X86,
    X86_64,
    Xcore,
    Xtensa,
    // LLVM tags deliberately omitted:
    // - aarch64_32
    // - amdil
    // - amdil64
    // - dxil
    // - le32
    // - le64
    // - r600
    // - hsail
    // - hsail64
    // - renderscript32
    // - renderscript64
    // - shave
    // - sparcel
    // - spir
    // - spir64
    // - tce
    // - tcele
}

impl FromStr for Arch {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            | "amdgcn" => Arch::AmdGcn,
            | "arc" => Arch::Arc,
            | "arm" => Arch::Arm,
            | "armeb" => Arch::Armeb,
            | "thumb" => Arch::Thumb,
            | "thumbeb" => Arch::Thumbeb,
            | "aarch64" => Arch::Aarch64,
            | "aarch64_be" => Arch::Aarch64be,
            | "avr" => Arch::Avr,
            | "bpfel" => Arch::Bpfel,
            | "bpfeb" => Arch::Bpfeb,
            | "csky" => Arch::Csky,
            | "hexagon" => Arch::Hexagon,
            | "kalimba" => Arch::Kalimba,
            | "lanai" => Arch::Lanai,
            | "loongarch32" => Arch::LoongArch32,
            | "loongarch64" => Arch::LoongArch64,
            | "m68k" => Arch::M68k,
            | "mips" => Arch::Mips,
            | "mipsel" => Arch::Mipsel,
            | "mips64" => Arch::Mips64,
            | "mips64el" => Arch::Mips64el,
            | "msp430" => Arch::Msp430,
            | "nvptx" => Arch::Nvptx,
            | "nvptx64" => Arch::Nvptx64,
            | "powerpc" => Arch::PowerPc,
            | "powerpcle" => Arch::PowerPcle,
            | "powerpc64" => Arch::PowerPc64,
            | "powerpc64le" => Arch::PowerPc64le,
            | "propeller" => Arch::Propeller,
            | "riscv32" => Arch::Riscv32,
            | "riscv64" => Arch::Riscv64,
            | "s390x" => Arch::S390x,
            | "sparc" => Arch::Sparc,
            | "sparc64" => Arch::Sparc64,
            | "spirv" => Arch::Spirv,
            | "spirv32" => Arch::Spirv32,
            | "spirv64" => Arch::Spirv64,
            | "ve" => Arch::Ve,
            | "wasm32" => Arch::Wasm32,
            | "wasm64" => Arch::Wasm64,
            | "x86" => Arch::X86,
            | "x86_64" => Arch::X86_64,
            | "xcore" => Arch::Xcore,
            | "xtensa" => Arch::Xtensa,

            | _ => return Err(()),
        })
    }
}

impl Arch {
    pub fn plan9_extension(&self) -> &'static str {
        match self {
            | Arch::Arm => ".5",
            | Arch::X86_64 => ".6",
            | Arch::Aarch64 => ".7",
            | Arch::X86 => ".8",
            | Arch::Sparc => ".k",
            | Arch::PowerPc | Arch::PowerPc64le => ".q",
            | Arch::Mips | Arch::Mipsel => ".v",

            // ISAs without designated characters get 'X' for lack of a better option.
            | _ => ".X",
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Abi {
    None,
    Gnu,
    GnuAbiN32,
    GnuAbi64,
    GnuEAbi,
    GnuEAbiHf,
    Gnuf32,
    Gnusf,
    GnuX32,
    Gnuilp32,
    Code16,
    EAbi,
    EAbiHf,
    Ilp32,
    Android,
    AndroidEAbi,
    Musl,
    MuslAbiN32,
    MuslAbi64,
    MuslEAbi,
    MuslEAbiHf,
    MuslX32,
    Msvc,
    Itanium,
    Cygnus,
    Simulator,
    MacAbi,
    Ohos,
    OhosEAbi,
    // LLVM tags deliberately omitted:
    // - amplification
    // - anyhit
    // - callable
    // - closesthit
    // - compute
    // - coreclr
    // - domain
    // - geometry
    // - gnuf64
    // - hull
    // - intersection
    // - library
    // - mesh
    // - miss
    // - pixel
    // - raygeneration
    // - vertex
}

impl FromStr for Abi {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            | "none" => Abi::None,
            | "gnu" => Abi::Gnu,
            | "gnuabin32" => Abi::GnuAbiN32,
            | "gnuabi64" => Abi::GnuAbi64,
            | "gnueabi" => Abi::GnuEAbi,
            | "gnueabihf" => Abi::GnuEAbiHf,
            | "gnuf32" => Abi::Gnuf32,
            | "gnusf" => Abi::Gnusf,
            | "gnux32" => Abi::GnuX32,
            | "gnuilp32" => Abi::Gnuilp32,
            | "code16" => Abi::Code16,
            | "eabi" => Abi::EAbi,
            | "eabihf" => Abi::EAbiHf,
            | "ilp32" => Abi::Ilp32,
            | "android" => Abi::Android,
            | "androideabi" => Abi::AndroidEAbi,
            | "musl" => Abi::Musl,
            | "muslabin32" => Abi::MuslAbiN32,
            | "muslabi64" => Abi::MuslAbi64,
            | "musleabi" => Abi::MuslEAbi,
            | "musleabihf" => Abi::MuslEAbiHf,
            | "muslx32" => Abi::MuslX32,
            | "msvc" => Abi::Msvc,
            | "itanium" => Abi::Itanium,
            | "cygnus" => Abi::Cygnus,
            | "simulator" => Abi::Simulator,
            | "macabi" => Abi::MacAbi,
            | "ohos" => Abi::Ohos,
            | "ohoseabi" => Abi::OhosEAbi,

            | _ => return Err(()),
        })
    }
}

impl Abi {
    pub fn default(arch: Arch, os: Os) -> Abi {
        match os {
            | Os::Freestanding | Os::Other => match arch {
                // Soft float is usually a sane default for freestanding
                | Arch::Arm
                | Arch::Armeb
                | Arch::Thumb
                | Arch::Thumbeb
                | Arch::Csky
                | Arch::Mips
                | Arch::Mipsel
                | Arch::PowerPc
                | Arch::PowerPcle => Abi::EAbi,

                | _ => Abi::None,
            },

            | Os::Aix => {
                if arch == Arch::PowerPc {
                    Abi::EAbiHf
                } else {
                    Abi::None
                }
            }

            | Os::Haiku => match arch {
                | Arch::Arm | Arch::Thumb | Arch::PowerPc => Abi::EAbiHf,

                | _ => Abi::None,
            },

            | Os::Hurd => Abi::Gnu,

            | Os::Linux => match arch {
                | Arch::Arm | Arch::Armeb | Arch::Thumb | Arch::Thumbeb | Arch::PowerPc | Arch::PowerPcle => {
                    Abi::MuslEAbiHf
                }

                // Soft float tends to be more common for CSKY and MIPS
                | Arch::Csky => Abi::GnuEAbi, // No musl support
                | Arch::Mips | Arch::Mipsel => Abi::MuslEAbi,
                | Arch::Mips64 | Arch::Mips64el => Abi::MuslAbi64,

                | _ => Abi::Musl,
            },

            | Os::Rtems => match arch {
                | Arch::Arm | Arch::Armeb | Arch::Thumb | Arch::Thumbeb | Arch::Mips | Arch::Mipsel => {
                    Abi::EAbi
                }

                | Arch::PowerPc => Abi::EAbiHf,

                | _ => Abi::None,
            },

            | Os::FreeBsd => match arch {
                | Arch::Arm | Arch::Armeb | Arch::Thumb | Arch::Thumbeb | Arch::PowerPc => Abi::EAbiHf,

                // Soft float tends to be more common for MIPS
                | Arch::Mips | Arch::Mipsel => Abi::EAbi,

                | _ => Abi::None,
            },

            | Os::NetBsd => match arch {
                | Arch::Arm | Arch::Armeb | Arch::Thumb | Arch::Thumbeb | Arch::PowerPc => Abi::EAbiHf,

                // Soft float tends to be more common for MIPS
                | Arch::Mips | Arch::Mipsel => Abi::EAbi,

                | _ => Abi::None,
            },

            | Os::OpenBsd => match arch {
                | Arch::Arm | Arch::Thumb => Abi::EAbi,
                | Arch::PowerPc => Abi::EAbiHf,
                | _ => Abi::None,
            },

            | Os::Ios => {
                if arch == Arch::X86_64 {
                    Abi::MacAbi
                } else {
                    Abi::None
                }
            }

            | Os::TvOs | Os::VisionOs | Os::WatchOs => {
                if arch == Arch::X86_64 {
                    Abi::Simulator
                } else {
                    Abi::None
                }
            }

            | Os::Windows => Abi::Gnu,

            | Os::Uefi => Abi::Msvc,

            | Os::Wasi | Os::Emscripten => Abi::Musl,

            | Os::Contiki
            | Os::Elfiamcu
            | Os::Fuchsia
            | Os::Hermit
            | Os::Plan9
            | Os::Serenity
            | Os::Zos
            | Os::Dragonfly
            | Os::Driverkit
            | Os::MacOs
            | Os::Illumos
            | Os::Solaris
            | Os::Ps3
            | Os::Ps4
            | Os::Ps5
            | Os::Amdhsa
            | Os::Amdpal
            | Os::Cuda
            | Os::Mesa3d
            | Os::Nvcl
            | Os::Opencl
            | Os::Opengl
            | Os::Vulkan => Abi::None,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ObjectFormat {
    /// The Common Object File Format used by Windows and UEFI.
    Coff,
    /// The Executable and Linkable Format used by many Unixes.
    Elf,
    /// The Generalized Object File Format used by z/OS.
    Goff,
    /// The Intel HEX format for storing binary code in ASCII text.
    Hex,
    /// The Mach object format used by macOS and other Apple platforms.
    Macho,
    /// Nvidia's PTX (Parallel Thread Execution) assembly language.
    Nvptx,
    /// The a.out format used by Plan 9 from Bell Labs.
    Plan9,
    /// Machine code with no metadata.
    Raw,
    /// The Khronos Group's Standard Portable Intermediate Representation V.
    Spirv,
    /// The WebAssembly binary format.
    Wasm,
    /// The eXtended Common Object File Format used by AIX.
    XCoff,
    // LLVM tags deliberately omitted:
    // - dxcontainer
}

impl ObjectFormat {
    pub fn file_extension(&self, arch: Arch) -> &'static str {
        match self {
            | ObjectFormat::Coff => "obj",
            | ObjectFormat::Elf
            | ObjectFormat::Goff
            | ObjectFormat::Macho
            | ObjectFormat::Wasm
            | ObjectFormat::XCoff => "o",
            | ObjectFormat::Hex => "ihex",
            | ObjectFormat::Nvptx => "ptx",
            | ObjectFormat::Plan9 => arch.plan9_extension(),
            | ObjectFormat::Raw => "bin",
            | ObjectFormat::Spirv => "spv",
        }
    }

    pub fn default(arch: Arch, os: Os) -> ObjectFormat {
        match os {
            | Os::Aix => ObjectFormat::XCoff,

            | Os::Driverkit | Os::Ios | Os::MacOs | Os::TvOs | Os::VisionOs | Os::WatchOs => {
                ObjectFormat::Macho
            }

            | Os::Plan9 => ObjectFormat::Plan9,

            | Os::Uefi | Os::Windows => ObjectFormat::Coff,

            | Os::Zos => ObjectFormat::Goff,

            | _ => match arch {
                | Arch::Nvptx | Arch::Nvptx64 => ObjectFormat::Nvptx,
                | Arch::Spirv | Arch::Spirv64 | Arch::Spirv32 => ObjectFormat::Spirv,
                | Arch::Wasm32 | Arch::Wasm64 => ObjectFormat::Wasm,

                | _ => ObjectFormat::Elf,
            },
        }
    }
}

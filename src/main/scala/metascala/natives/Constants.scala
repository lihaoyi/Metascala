package metascala.natives

/**
  * Copy-pasted version of java.lang.invoke.MethodHandleNatives.Constants,
  * which isn't public for us to use directly
  */
object MethodHandleNativeConstants { // MethodHandleImpl
  val GC_COUNT_GWT = 4
  val GC_LAMBDA_SUPPORT = 5
  // MemberName
  // The JVM uses values of -2 and above for vtable indexes.
  // Field values are simple positive offsets.
  // Ref: src/share/vm/oops/methodOop.hpp
  // This value is negative enough to avoid such numbers,
  // but not too negative.
  val MN_IS_METHOD = 0x00010000
  // method (not constructor)
  val MN_IS_CONSTRUCTOR = 0x00020000
  // constructor
  val MN_IS_FIELD = 0x00040000
  // field
  val MN_IS_TYPE = 0x00080000
  // nested type
  val MN_CALLER_SENSITIVE = 0x00100000
  // @CallerSensitive annotation detected
  val MN_REFERENCE_KIND_SHIFT = 24
  // refKind
  val MN_REFERENCE_KIND_MASK = 0x0F000000 >> MN_REFERENCE_KIND_SHIFT
  // The SEARCH_* bits are not for MN.flags but for the matchFlags argument of MHN.getMembers:
  val MN_SEARCH_SUPERCLASSES = 0x00100000
  val MN_SEARCH_INTERFACES = 0x00200000
  /**
    * Basic types as encoded in the JVM.  These code values are not
    * intended for use outside this class.  They are used as part of
    * a private interface between the JVM and this class.
    */
  val T_BOOLEAN = 4
  val T_CHAR = 5
  val T_FLOAT = 6
  val T_DOUBLE = 7
  val T_BYTE = 8
  val T_SHORT = 9
  val T_INT = 10
  val T_LONG = 11
  val T_OBJECT = 12
  //T_ARRAY    = 13
  val T_VOID = 14
  //T_ADDRESS  = 15
  val T_ILLEGAL = 99
  /**
    * Constant pool entry types.
    */
  val CONSTANT_Utf8 = 1
  val CONSTANT_Integer = 3
  val CONSTANT_Float = 4
  val CONSTANT_Long = 5
  val CONSTANT_Double = 6
  val CONSTANT_Class = 7
  val CONSTANT_String = 8
  val CONSTANT_Fieldref = 9
  val CONSTANT_Methodref = 10
  val CONSTANT_InterfaceMethodref = 11
  val CONSTANT_NameAndType = 12
  val CONSTANT_MethodHandle = 15
  // JSR 292
  val CONSTANT_MethodType = 16
  val CONSTANT_InvokeDynamic = 18
  val CONSTANT_LIMIT = 19 // Limit to tags found in classfiles

  /**
    * Access modifier flags.
    */
  val ACC_PUBLIC = 0x0001
  val ACC_PRIVATE = 0x0002
  val ACC_PROTECTED = 0x0004
  val ACC_STATIC = 0x0008
  val ACC_FINAL = 0x0010
  val ACC_SYNCHRONIZED = 0x0020
  val ACC_VOLATILE = 0x0040
  val ACC_TRANSIENT = 0x0080
  val ACC_NATIVE = 0x0100
  val ACC_INTERFACE = 0x0200
  val ACC_ABSTRACT = 0x0400
  val ACC_STRICT = 0x0800
  val ACC_SYNTHETIC = 0x1000
  val ACC_ANNOTATION = 0x2000
  val ACC_ENUM = 0x4000
  // aliases:
  val ACC_SUPER = ACC_SYNCHRONIZED
  val ACC_BRIDGE = ACC_VOLATILE
  val ACC_VARARGS = ACC_TRANSIENT
  /**
    * Constant pool reference-kind codes, as used by CONSTANT_MethodHandle CP entries.
    */
  val REF_NONE = 0
  // null value
  val REF_getField = 1
  val REF_getStatic = 2
  val REF_putField = 3
  val REF_putStatic = 4
  val REF_invokeVirtual = 5
  val REF_invokeStatic = 6
  val REF_invokeSpecial = 7
  val REF_newInvokeSpecial = 8
  val REF_invokeInterface = 9
  val REF_LIMIT = 10
}
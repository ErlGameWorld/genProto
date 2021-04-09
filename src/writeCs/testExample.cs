using System.Collections.Generic;
public enum EnumName : byte
{
	EN1,
	EN2,
	EN3,
}
namespace bintalk
{
	public static partial class ProtocolReader
	{
		public static bool read(bintalk.IReader r, ref EnumName v, uint maxValue)
		{
			byte e = 0;
			if(!read(r, ref e, 0)) return false;
			v = (EnumName)e;
			return true;
		}
	}
	public static partial class ProtocolWriter
	{
		public static void write(bintalk.IWriter w, EnumName v)
		{
			write(w, (byte)v);
		}
	}
}
public enum EnumName16 : ushort
{
	EN16_a01,
	EN16_a02,
	EN16_a03,
	EN16_a04,
	EN16_a05,
	EN16_a06,
	EN16_a07,
	EN16_a08,
	EN16_a11,
	EN16_a12,
	EN16_a13,
	EN16_a14,
	EN16_a15,
	EN16_a16,
	EN16_a17,
	EN16_a18,
	EN16_a21,
	EN16_a22,
	EN16_a23,
	EN16_a24,
	EN16_a25,
	EN16_a26,
	EN16_a27,
	EN16_a28,
	EN16_a31,
	EN16_a32,
	EN16_a33,
	EN16_a34,
	EN16_a35,
	EN16_a36,
	EN16_a37,
	EN16_a38,
	EN16_a41,
	EN16_a42,
	EN16_a43,
	EN16_a44,
	EN16_a45,
	EN16_a46,
	EN16_a47,
	EN16_a48,
	EN16_a51,
	EN16_a52,
	EN16_a53,
	EN16_a54,
	EN16_a55,
	EN16_a56,
	EN16_a57,
	EN16_a58,
	EN16_a61,
	EN16_a62,
	EN16_a63,
	EN16_a64,
	EN16_a65,
	EN16_a66,
	EN16_a67,
	EN16_a68,
	EN16_a71,
	EN16_a72,
	EN16_a73,
	EN16_a74,
	EN16_a75,
	EN16_a76,
	EN16_a77,
	EN16_a78,
	EN16_b01,
	EN16_b02,
	EN16_b03,
	EN16_b04,
	EN16_b05,
	EN16_b06,
	EN16_b07,
	EN16_b08,
	EN16_b11,
	EN16_b12,
	EN16_b13,
	EN16_b14,
	EN16_b15,
	EN16_b16,
	EN16_b17,
	EN16_b18,
	EN16_b21,
	EN16_b22,
	EN16_b23,
	EN16_b24,
	EN16_b25,
	EN16_b26,
	EN16_b27,
	EN16_b28,
	EN16_b31,
	EN16_b32,
	EN16_b33,
	EN16_b34,
	EN16_b35,
	EN16_b36,
	EN16_b37,
	EN16_b38,
	EN16_b41,
	EN16_b42,
	EN16_b43,
	EN16_b44,
	EN16_b45,
	EN16_b46,
	EN16_b47,
	EN16_b48,
	EN16_b51,
	EN16_b52,
	EN16_b53,
	EN16_b54,
	EN16_b55,
	EN16_b56,
	EN16_b57,
	EN16_b58,
	EN16_b61,
	EN16_b62,
	EN16_b63,
	EN16_b64,
	EN16_b65,
	EN16_b66,
	EN16_b67,
	EN16_b68,
	EN16_b71,
	EN16_b72,
	EN16_b73,
	EN16_b74,
	EN16_b75,
	EN16_b76,
	EN16_b77,
	EN16_b78,
	EN16_c01,
	EN16_c02,
	EN16_c03,
	EN16_c04,
	EN16_c05,
	EN16_c06,
	EN16_c07,
	EN16_c08,
	EN16_c11,
	EN16_c12,
	EN16_c13,
	EN16_c14,
	EN16_c15,
	EN16_c16,
	EN16_c17,
	EN16_c18,
	EN16_c21,
	EN16_c22,
	EN16_c23,
	EN16_c24,
	EN16_c25,
	EN16_c26,
	EN16_c27,
	EN16_c28,
	EN16_c31,
	EN16_c32,
	EN16_c33,
	EN16_c34,
	EN16_c35,
	EN16_c36,
	EN16_c37,
	EN16_c38,
	EN16_c41,
	EN16_c42,
	EN16_c43,
	EN16_c44,
	EN16_c45,
	EN16_c46,
	EN16_c47,
	EN16_c48,
	EN16_c51,
	EN16_c52,
	EN16_c53,
	EN16_c54,
	EN16_c55,
	EN16_c56,
	EN16_c57,
	EN16_c58,
	EN16_c61,
	EN16_c62,
	EN16_c63,
	EN16_c64,
	EN16_c65,
	EN16_c66,
	EN16_c67,
	EN16_c68,
	EN16_c71,
	EN16_c72,
	EN16_c73,
	EN16_c74,
	EN16_c75,
	EN16_c76,
	EN16_c77,
	EN16_c78,
	EN16_d01,
	EN16_d02,
	EN16_d03,
	EN16_d04,
	EN16_d05,
	EN16_d06,
	EN16_d07,
	EN16_d08,
	EN16_d11,
	EN16_d12,
	EN16_d13,
	EN16_d14,
	EN16_d15,
	EN16_d16,
	EN16_d17,
	EN16_d18,
	EN16_d21,
	EN16_d22,
	EN16_d23,
	EN16_d24,
	EN16_d25,
	EN16_d26,
	EN16_d27,
	EN16_d28,
	EN16_d31,
	EN16_d32,
	EN16_d33,
	EN16_d34,
	EN16_d35,
	EN16_d36,
	EN16_d37,
	EN16_d38,
	EN16_d41,
	EN16_d42,
	EN16_d43,
	EN16_d44,
	EN16_d45,
	EN16_d46,
	EN16_d47,
	EN16_d48,
	EN16_d51,
	EN16_d52,
	EN16_d53,
	EN16_d54,
	EN16_d55,
	EN16_d56,
	EN16_d57,
	EN16_d58,
	EN16_d61,
	EN16_d62,
	EN16_d63,
	EN16_d64,
	EN16_d65,
	EN16_d66,
	EN16_d67,
	EN16_d68,
	EN16_d71,
	EN16_d72,
	EN16_d73,
	EN16_d74,
	EN16_d75,
	EN16_d76,
	EN16_d77,
	EN16_d78,
	Max,
}
namespace bintalk
{
	public static partial class ProtocolReader
	{
		public static bool read(bintalk.IReader r, ref EnumName16 v, uint maxValue)
		{
			ushort e = 0;
			if(!read(r, ref e, 0)) return false;
			v = (EnumName16)e;
			return true;
		}
	}
	public static partial class ProtocolWriter
	{
		public static void write(bintalk.IWriter w, EnumName16 v)
		{
			write(w, (ushort)v);
		}
	}
}
public class StructType
{
	public string aaa_= "";
	public int bbb_= 0;
	public void serialize(bintalk.IWriter __w__)
	{
		bintalk.ProtocolWriter.write(__w__, aaa_);
		bintalk.ProtocolWriter.write(__w__, bbb_);
	}
	public bool deserialize(bintalk.IReader __r__)
	{
		if(!bintalk.ProtocolReader.read(__r__, ref aaa_, 0XFFFFFFFF)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref bbb_, 0X0)) return false;
		return true;
	}
}
namespace bintalk
{
	public static partial class ProtocolReader
	{
		public static bool read(bintalk.IReader r, ref StructType v, uint maxValue)
		{
			return v.deserialize(r);
		}
	}
	public static partial class ProtocolWriter
	{
		public static void write(bintalk.IWriter w, StructType v)
		{
			v.serialize(w);
		}
	}
}
public class StructBase
{
	public double double_= 0;
	public float float_= 0;
	public long int64_= 0;
	public ulong uint64_= 0;
	public int int32_= 0;
	public uint uint32_= 0;
	public short int16_= 0;
	public ushort uint16_= 0;
	public sbyte int8_= 0;
	public byte uint8_= 0;
	public bool bool_= false;
	public EnumName enum_= (EnumName)(0);
	public EnumName16 enum16_= (EnumName16)(0);
	public StructType struct_= new StructType();
	public string string_= "";
	public string string1_= "";
	public byte[] bin_= new byte[0];
	public byte[] bin1_= new byte[0];
	public List<double> doubleArray_= new List<double>();
	public List<float> floatArray_= new List<float>();
	public List<long> int64Array_= new List<long>();
	public List<ulong> uint64Array_= new List<ulong>();
	public List<int> int32Array_= new List<int>();
	public List<uint> uint32Array_= new List<uint>();
	public List<short> int16Array_= new List<short>();
	public List<ushort> uint16Array_= new List<ushort>();
	public List<sbyte> int8Array_= new List<sbyte>();
	public List<byte> uint8Array_= new List<byte>();
	public List<bool> boolArray_= new List<bool>();
	public List<string> strArray_= new List<string>();
	public List<string> strArray1_= new List<string>();
	public List<byte[]> binArray_= new List<byte[]>();
	public List<byte[]> bin1Array_= new List<byte[]>();
	public List<EnumName> enumArray_= new List<EnumName>();
	public List<StructType> structArray_= new List<StructType>();
	public void serialize(bintalk.IWriter __w__)
	{
		bintalk.ProtocolWriter.write(__w__, double_);
		bintalk.ProtocolWriter.write(__w__, float_);
		bintalk.ProtocolWriter.write(__w__, int64_);
		bintalk.ProtocolWriter.write(__w__, uint64_);
		bintalk.ProtocolWriter.write(__w__, int32_);
		bintalk.ProtocolWriter.write(__w__, uint32_);
		bintalk.ProtocolWriter.write(__w__, int16_);
		bintalk.ProtocolWriter.write(__w__, uint16_);
		bintalk.ProtocolWriter.write(__w__, int8_);
		bintalk.ProtocolWriter.write(__w__, uint8_);
		bintalk.ProtocolWriter.write(__w__, bool_);
		bintalk.ProtocolWriter.write(__w__, enum_);
		bintalk.ProtocolWriter.write(__w__, enum16_);
		bintalk.ProtocolWriter.write(__w__, struct_);
		bintalk.ProtocolWriter.write(__w__, string_);
		bintalk.ProtocolWriter.write(__w__, string1_);
		bintalk.ProtocolWriter.write(__w__, bin_);
		bintalk.ProtocolWriter.write(__w__, bin1_);
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)doubleArray_.Count);
			foreach(double __vi__ in doubleArray_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)floatArray_.Count);
			foreach(float __vi__ in floatArray_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)int64Array_.Count);
			foreach(long __vi__ in int64Array_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)uint64Array_.Count);
			foreach(ulong __vi__ in uint64Array_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)int32Array_.Count);
			foreach(int __vi__ in int32Array_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)uint32Array_.Count);
			foreach(uint __vi__ in uint32Array_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)int16Array_.Count);
			foreach(short __vi__ in int16Array_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)uint16Array_.Count);
			foreach(ushort __vi__ in uint16Array_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)int8Array_.Count);
			foreach(sbyte __vi__ in int8Array_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)uint8Array_.Count);
			foreach(byte __vi__ in uint8Array_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)boolArray_.Count);
			foreach(bool __vi__ in boolArray_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)strArray_.Count);
			foreach(string __vi__ in strArray_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)strArray1_.Count);
			foreach(string __vi__ in strArray1_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)binArray_.Count);
			foreach(byte[] __vi__ in binArray_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)bin1Array_.Count);
			foreach(byte[] __vi__ in bin1Array_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)enumArray_.Count);
			foreach(EnumName __vi__ in enumArray_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
		{
			bintalk.ProtocolWriter.writeDynSize(__w__, (uint)structArray_.Count);
			foreach(StructType __vi__ in structArray_) bintalk.ProtocolWriter.write(__w__, __vi__);
		}
	}
	public bool deserialize(bintalk.IReader __r__)
	{
		if(!bintalk.ProtocolReader.read(__r__, ref double_, 0X0)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref float_, 0X0)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref int64_, 0X0)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref uint64_, 0X0)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref int32_, 0X0)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref uint32_, 0X0)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref int16_, 0X0)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref uint16_, 0X0)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref int8_, 0X0)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref uint8_, 0X0)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref bool_, 0X0)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref enum_, 0X2)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref enum16_, 0X100)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref struct_, 0X0)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref string_, 0XFFFFFFFF)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref string1_, 0X20)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref bin_, 0XFFFFFFFF)) return false;
		if(!bintalk.ProtocolReader.read(__r__, ref bin1_, 0X20)) return false;
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				double __vi__ = 0;
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X0)) return false;
				doubleArray_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				float __vi__ = 0;
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X0)) return false;
				floatArray_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				long __vi__ = 0;
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X0)) return false;
				int64Array_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				ulong __vi__ = 0;
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X0)) return false;
				uint64Array_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				int __vi__ = 0;
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X0)) return false;
				int32Array_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				uint __vi__ = 0;
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X0)) return false;
				uint32Array_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				short __vi__ = 0;
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X0)) return false;
				int16Array_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				ushort __vi__ = 0;
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X0)) return false;
				uint16Array_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				sbyte __vi__ = 0;
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X0)) return false;
				int8Array_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				byte __vi__ = 0;
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X0)) return false;
				uint8Array_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				bool __vi__ = false;
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X0)) return false;
				boolArray_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				string __vi__ = "";
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0XFFFFFFFF)) return false;
				strArray_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0X8) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				string __vi__ = "";
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X10)) return false;
				strArray1_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				byte[] __vi__ = new byte[0];
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0XFFFFFFFF)) return false;
				binArray_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				byte[] __vi__ = new byte[0];
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X20)) return false;
				bin1Array_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				EnumName __vi__ = (EnumName)(0);
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X2)) return false;
				enumArray_.Add(__vi__);
			}
		}
		{
			uint __s__;
			if (!bintalk.ProtocolReader.readDynSize(__r__, out __s__) || __s__ > 0XFFFFFFFF) return false;
			for(uint __i__ = 0; __i__ < __s__; __i__++)
			{
				StructType __vi__ = new StructType();
				if (!bintalk.ProtocolReader.read(__r__, ref __vi__, 0X0)) return false;
				structArray_.Add(__vi__);
			}
		}
		return true;
	}
}
namespace bintalk
{
	public static partial class ProtocolReader
	{
		public static bool read(bintalk.IReader r, ref StructBase v, uint maxValue)
		{
			return v.deserialize(r);
		}
	}
	public static partial class ProtocolWriter
	{
		public static void write(bintalk.IWriter w, StructBase v)
		{
			v.serialize(w);
		}
	}
}

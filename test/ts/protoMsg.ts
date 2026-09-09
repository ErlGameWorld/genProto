const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder();

export class ByteArray {
    private buffer: Uint8Array;
    private view: DataView;
    private position: number = 0;
    private capacity: number;

    constructor(initialCapacity: number = 2048) {
        this.capacity = initialCapacity;
        this.buffer = new Uint8Array(initialCapacity);
        this.view = new DataView(this.buffer.buffer);
    }

    private ensureCapacity(bytes: number): void {
        if (this.position + bytes > this.capacity) {
            let newCapacity = this.capacity * 2;
            while (this.position + bytes > newCapacity) {
                newCapacity *= 2;
            }
            const newBuffer = new Uint8Array(newCapacity);
            newBuffer.set(this.buffer);
            this.buffer = newBuffer;
            this.view = new DataView(this.buffer.buffer);
            this.capacity = newCapacity;
        }
    }

    private checkAvailable(bytes: number): void {
        if (this.position + bytes > this.buffer.length) {
            throw new Error(`Buffer underflow: need ${bytes} bytes, available ${this.buffer.length - this.position}`);
        }
    }

    public write_bool(value: boolean): void {
        this.ensureCapacity(1);
        this.buffer[this.position++] = value ? 1 : 0;
    }

    public write_int8(value: number): void {
        if (value < -128 || value > 127) {
            throw new Error(`int8 value out of range: ${value}`);
        }
        this.ensureCapacity(1);
        this.view.setInt8(this.position++, value);
    }

    public write_uint8(value: number): void {
        if (value < 0 || value > 255) {
            throw new Error(`uint8 value out of range: ${value}`);
        }
        this.ensureCapacity(1);
        this.buffer[this.position++] = value;
    }

    public write_int16(value: number): void {
        if (value < -32768 || value > 32767) {
            throw new Error(`int16 value out of range: ${value}`);
        }
        this.ensureCapacity(2);
        this.view.setInt16(this.position, value, false); // big endian
        this.position += 2;
    }

    public write_uint16(value: number): void {
        if (value < 0 || value > 65535) {
            throw new Error(`uint16 value out of range: ${value}`);
        }
        this.ensureCapacity(2);
        this.view.setUint16(this.position, value, false);
        this.position += 2;
    }

    public write_int32(value: number): void {
        if (value < -2147483648 || value > 2147483647) {
            throw new Error(`int32 value out of range: ${value}`);
        }
        this.ensureCapacity(4);
        this.view.setInt32(this.position, value, false);
        this.position += 4;
    }

    public write_uint32(value: number): void {
        if (value < 0 || value > 4294967295) {
            throw new Error(`uint32 value out of range: ${value}`);
        }
        this.ensureCapacity(4);
        this.view.setUint32(this.position, value, false);
        this.position += 4;
    }

    public write_int64(value: bigint): void {
        this.ensureCapacity(8);
        this.view.setBigInt64(this.position, value, false);
        this.position += 8;
    }

    public write_uint64(value: bigint): void {
        this.ensureCapacity(8);
        this.view.setBigUint64(this.position, value, false);
        this.position += 8;
    }

    public write_float(value: number): void {
        if (isNaN(value) || !isFinite(value)) {
            throw new Error(`Invalid float value: ${value}`);
        }
        this.ensureCapacity(4);
        this.view.setFloat32(this.position, value, false);
        this.position += 4;
    }

    public write_double(value: number): void {
        if (isNaN(value) || !isFinite(value)) {
            throw new Error(`Invalid double value: ${value}`);
        }
        this.ensureCapacity(8);
        this.view.setFloat64(this.position, value, false);
        this.position += 8;
    }

    public write_string(value: string): void {
        if (!value) value = '';
        
        const maxLength = value.length * 4;
        this.ensureCapacity(maxLength + 2);
        
        const lengthPos = this.position;
        this.position += 2;
        
        const result = textEncoder.encodeInto(value, this.buffer.subarray(this.position));
        const written = result.written || 0;
        
        if (written > 65535) {
            throw new Error(`String too long: ${written} bytes (max: 65535)`);
        }
        
        this.view.setUint16(lengthPos, written, false);
        this.position += written;
    }

    public write_binary(value: Uint8Array | null | undefined): void {
        const bin = value || new Uint8Array();
        if (bin.length > 65535) {
            throw new Error(`Binary too long: ${bin.length} bytes (max: 65535)`);
        }
        this.write_uint16(bin.length);
        this.ensureCapacity(bin.length);
        this.buffer.set(bin, this.position);
        this.position += bin.length;
    }

    public write_integer(value: number): void {
        if (!Number.isInteger(value)) {
            throw new Error(`write_integer: value must be an integer, got ${value}`);
        }
        if (value >= -128 && value <= 127) {
            this.write_uint8(8);
            this.write_int8(value);
        } else if (value >= -32768 && value <= 32767) {
            this.write_uint8(16);
            this.write_int16(value);
        } else if (value >= -2147483648 && value <= 2147483647) {
            this.write_uint8(32);
            this.write_int32(value);
        } else {
            this.write_uint8(64);
            this.write_int64(BigInt(value));
        }
    }

    public write_number(value: number): void {
        if (Number.isInteger(value)) {
            this.write_integer(value);
        } else {
            if (Math.fround(value) === value) {
                this.write_uint8(33);
                this.write_float(value);
            } else {
                this.write_uint8(65);
                this.write_double(value);
            }
        }
    }

    public read_bool(): boolean {
        this.checkAvailable(1);
        return this.buffer[this.position++] !== 0;
    }

    public read_int8(): number {
        this.checkAvailable(1);
        return this.view.getInt8(this.position++);
    }

    public read_uint8(): number {
        this.checkAvailable(1);
        return this.buffer[this.position++];
    }

    public read_int16(): number {
        this.checkAvailable(2);
        const value = this.view.getInt16(this.position, false);
        this.position += 2;
        return value;
    }

    public read_uint16(): number {
        this.checkAvailable(2);
        const value = this.view.getUint16(this.position, false);
        this.position += 2;
        return value;
    }

    public read_int32(): number {
        this.checkAvailable(4);
        const value = this.view.getInt32(this.position, false);
        this.position += 4;
        return value;
    }

    public read_uint32(): number {
        this.checkAvailable(4);
        const value = this.view.getUint32(this.position, false);
        this.position += 4;
        return value;
    }

    public read_int64(): bigint {
        this.checkAvailable(8);
        const value = this.view.getBigInt64(this.position, false);
        this.position += 8;
        return value;
    }

    public read_uint64(): bigint {
        this.checkAvailable(8);
        const value = this.view.getBigUint64(this.position, false);
        this.position += 8;
        return value;
    }

    public read_float(): number {
        this.checkAvailable(4);
        const value = this.view.getFloat32(this.position, false);
        this.position += 4;
        return value;
    }

    public read_double(): number {
        this.checkAvailable(8);
        const value = this.view.getFloat64(this.position, false);
        this.position += 8;
        return value;
    }

    public read_string(): string {
        const length = this.read_uint16();
        this.checkAvailable(length);

        const str = textDecoder.decode(this.buffer.subarray(this.position, this.position + length));
        this.position += length;
        return str;
    }

    public read_binary(): Uint8Array {
        const length = this.read_uint16();
        this.checkAvailable(length);
        const bin = this.buffer.slice(this.position, this.position + length);
        this.position += length;
        return bin;
    }

    public read_integer(): number {
        const tag = this.read_uint8();
        switch (tag) {
            case 8: return this.read_int8();
            case 16: return this.read_int16();
            case 32: return this.read_int32();
            case 64: return Number(this.read_int64());
            default: throw new Error(`Unknown integer tag: ${tag}`);
        }
    }

    public read_number(): number {
        const tag = this.read_uint8();
        switch (tag) {
            case 8: return this.read_int8();
            case 16: return this.read_int16();
            case 32: return this.read_int32();
            case 64: return Number(this.read_int64());
            case 33: return this.read_float();
            case 65: return this.read_double();
            default: throw new Error(`Invalid number tag: ${tag}`);
        }
    }

    public write_list<T>(list: T[] | null | undefined, writer: (val: T) => void): void {
        if (!list) {
            this.write_uint16(0);
            return;
        }
        const length = list.length;
        if (length > 65535) {
            throw new Error(`List too long: ${length} (max: 65535)`);
        }
        this.write_uint16(length);
        for (let i = 0; i < length; i++) {
            writer(list[i]);
        }
    }

    public read_list<T>(reader: () => T): T[] {
        const length = this.read_uint16();
        const list = new Array<T>(length);
        for (let i = 0; i < length; i++) {
            list[i] = reader();
        }
        return list;
    }

    public write_map<K, V>(map: Map<K, V> | null | undefined, writeKey: (k: K) => void, writeVal: (v: V) => void): void {
        if (!map) {
            this.write_uint16(0);
            return;
        }
        const length = map.size;
        if (length > 65535) {
            throw new Error(`Map too long: ${length} (max: 65535)`);
        }
        this.write_uint16(length);
        for (const [k, v] of map) {
            writeKey(k);
            writeVal(v);
        }
    }

    public read_map<K, V>(readKey: () => K, readVal: () => V): Map<K, V> {
        const length = this.read_uint16();
        const map = new Map<K, V>();
        for (let i = 0; i < length; i++) {
            const k = readKey();
            const v = readVal();
            map.set(k, v);
        }
        return map;
    }

    public getBytes(): Uint8Array {
        return this.buffer.slice(0, this.position);
    }

    public getBytesAsArray(): number[] {
        return Array.from(this.buffer.subarray(0, this.position));
    }

    public setBytes(bytes: Uint8Array | number[], copy: boolean = false): void {
        if (bytes instanceof Uint8Array) {
            if (copy) {
                this.buffer = new Uint8Array(bytes.length);
                this.buffer.set(bytes);
            } else {
                this.buffer = bytes;
            }
            this.capacity = bytes.length;
        } else {
            this.buffer = new Uint8Array(bytes);
            this.capacity = bytes.length;
        }
        this.view = new DataView(this.buffer.buffer, this.buffer.byteOffset, this.buffer.byteLength);
        this.position = 0;
    }
}

export interface ProtoMessage {
    msgId: number;
    encode(byteArray: ByteArray): void;
    decode(byteArray: ByteArray): void;
}

export class test {
	public static readonly PROTO_ID = 1;
	public msgId: number = 1;
		public aa: string = "";

	public encode(byteArray: ByteArray): void {
			byteArray.write_string(this.aa);
	}

	public decode(byteArray: ByteArray): void {
			this.aa = byteArray.read_string();
	}
}

export class phoneNumber {
	public static readonly PROTO_ID = 2;
	public msgId: number = 2;
		public number: test | null = null;
		public type: number = 0;

	public encode(byteArray: ByteArray): void {
			if (this.number) {
				byteArray.write_uint8(1);
				this.number.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			byteArray.write_int32(this.type);
	}

	public decode(byteArray: ByteArray): void {
			const hasnumber = byteArray.read_uint8() > 0;
			if (hasnumber) {
				this.number = new test();
				this.number.decode(byteArray);
			} else {
				this.number = null;
			}
			this.type = byteArray.read_int32();
	}
}

export class person {
	public static readonly PROTO_ID = 3;
	public msgId: number = 3;
		public name: string = "";
		public id: number = 0;
		public email: string = "";
		public phone: phoneNumber[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_string(this.name);
			byteArray.write_int32(this.id);
			byteArray.write_string(this.email);
			byteArray.write_list(this.phone, (item) => item.encode(byteArray));
	}

	public decode(byteArray: ByteArray): void {
			this.name = byteArray.read_string();
			this.id = byteArray.read_int32();
			this.email = byteArray.read_string();
			this.phone = byteArray.read_list(() => {
				const item = new phoneNumber();
				item.decode(byteArray);
				return item;
			});
	}
}

export class addressBook {
	public static readonly PROTO_ID = 4;
	public msgId: number = 4;
		public person: person[] = [];
		public other: person[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.person, (item) => item.encode(byteArray));
			byteArray.write_list(this.other, (item) => item.encode(byteArray));
	}

	public decode(byteArray: ByteArray): void {
			this.person = byteArray.read_list(() => {
				const item = new person();
				item.decode(byteArray);
				return item;
			});
			this.other = byteArray.read_list(() => {
				const item = new person();
				item.decode(byteArray);
				return item;
			});
	}
}

export class union {
	public static readonly PROTO_ID = 5;
	public msgId: number = 5;
		public test: string = "";
		public type: number = 0;

	public encode(byteArray: ByteArray): void {
			byteArray.write_string(this.test);
			byteArray.write_int32(this.type);
	}

	public decode(byteArray: ByteArray): void {
			this.test = byteArray.read_string();
			this.type = byteArray.read_int32();
	}
}

export class tbool {
	public static readonly PROTO_ID = 6;
	public msgId: number = 6;
		public flag: boolean = false;

	public encode(byteArray: ByteArray): void {
			byteArray.write_bool(this.flag);
	}

	public decode(byteArray: ByteArray): void {
			this.flag = byteArray.read_bool();
	}
}

export class tint8 {
	public static readonly PROTO_ID = 7;
	public msgId: number = 7;
		public int1: number = 0;
		public int2: number = 0;

	public encode(byteArray: ByteArray): void {
			byteArray.write_int8(this.int1);
			byteArray.write_int8(this.int2);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_int8();
			this.int2 = byteArray.read_int8();
	}
}

export class tuint8 {
	public static readonly PROTO_ID = 8;
	public msgId: number = 8;
		public int1: number = 0;
		public int2: number = 0;

	public encode(byteArray: ByteArray): void {
			byteArray.write_uint8(this.int1);
			byteArray.write_uint8(this.int2);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_uint8();
			this.int2 = byteArray.read_uint8();
	}
}

export class tint16 {
	public static readonly PROTO_ID = 9;
	public msgId: number = 9;
		public int1: number = 0;
		public int2: number = 0;

	public encode(byteArray: ByteArray): void {
			byteArray.write_int16(this.int1);
			byteArray.write_int16(this.int2);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_int16();
			this.int2 = byteArray.read_int16();
	}
}

export class tuint16 {
	public static readonly PROTO_ID = 10;
	public msgId: number = 10;
		public int1: number = 0;
		public int2: number = 0;

	public encode(byteArray: ByteArray): void {
			byteArray.write_uint16(this.int1);
			byteArray.write_uint16(this.int2);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_uint16();
			this.int2 = byteArray.read_uint16();
	}
}

export class tint32 {
	public static readonly PROTO_ID = 11;
	public msgId: number = 11;
		public int1: number = 0;
		public int2: number = 0;
		public int3: number = 0;
		public int4: number = 0;
		public int5: number = 0;
		public int6: number = 0;
		public int7: number = 0;
		public int8: number = 0;
		public int9: number = 0;
		public int10: number = 0;

	public encode(byteArray: ByteArray): void {
			byteArray.write_int32(this.int1);
			byteArray.write_int32(this.int2);
			byteArray.write_int32(this.int3);
			byteArray.write_int32(this.int4);
			byteArray.write_int32(this.int5);
			byteArray.write_int32(this.int6);
			byteArray.write_int32(this.int7);
			byteArray.write_int32(this.int8);
			byteArray.write_int32(this.int9);
			byteArray.write_int32(this.int10);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_int32();
			this.int2 = byteArray.read_int32();
			this.int3 = byteArray.read_int32();
			this.int4 = byteArray.read_int32();
			this.int5 = byteArray.read_int32();
			this.int6 = byteArray.read_int32();
			this.int7 = byteArray.read_int32();
			this.int8 = byteArray.read_int32();
			this.int9 = byteArray.read_int32();
			this.int10 = byteArray.read_int32();
	}
}

export class tuint32 {
	public static readonly PROTO_ID = 12;
	public msgId: number = 12;
		public int1: number = 0;
		public int2: number = 0;

	public encode(byteArray: ByteArray): void {
			byteArray.write_uint32(this.int1);
			byteArray.write_uint32(this.int2);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_uint32();
			this.int2 = byteArray.read_uint32();
	}
}

export class tint64 {
	public static readonly PROTO_ID = 13;
	public msgId: number = 13;
		public int1: bigint = 0n;
		public int2: bigint = 0n;

	public encode(byteArray: ByteArray): void {
			byteArray.write_int64(this.int1);
			byteArray.write_int64(this.int2);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_int64();
			this.int2 = byteArray.read_int64();
	}
}

export class tuint64 {
	public static readonly PROTO_ID = 14;
	public msgId: number = 14;
		public int1: bigint = 0n;
		public int2: bigint = 0n;

	public encode(byteArray: ByteArray): void {
			byteArray.write_uint64(this.int1);
			byteArray.write_uint64(this.int2);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_uint64();
			this.int2 = byteArray.read_uint64();
	}
}

export class tinteger {
	public static readonly PROTO_ID = 15;
	public msgId: number = 15;
		public int1: number = 0;
		public int2: number = 0;
		public int3: number = 0;
		public int4: number = 0;
		public int5: number = 0;
		public int6: number = 0;
		public int7: number = 0;
		public int8: number = 0;

	public encode(byteArray: ByteArray): void {
			byteArray.write_integer(this.int1);
			byteArray.write_integer(this.int2);
			byteArray.write_integer(this.int3);
			byteArray.write_integer(this.int4);
			byteArray.write_integer(this.int5);
			byteArray.write_integer(this.int6);
			byteArray.write_integer(this.int7);
			byteArray.write_integer(this.int8);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_integer();
			this.int2 = byteArray.read_integer();
			this.int3 = byteArray.read_integer();
			this.int4 = byteArray.read_integer();
			this.int5 = byteArray.read_integer();
			this.int6 = byteArray.read_integer();
			this.int7 = byteArray.read_integer();
			this.int8 = byteArray.read_integer();
	}
}

export class tnumber {
	public static readonly PROTO_ID = 16;
	public msgId: number = 16;
		public int1: number = 0;
		public int2: number = 0;
		public int3: number = 0;
		public int4: number = 0;
		public int5: number = 0;
		public int6: number = 0;
		public int7: number = 0;
		public int8: number = 0;
		public float1: number = 0;
		public float2: number = 0;

	public encode(byteArray: ByteArray): void {
			byteArray.write_number(this.int1);
			byteArray.write_number(this.int2);
			byteArray.write_number(this.int3);
			byteArray.write_number(this.int4);
			byteArray.write_number(this.int5);
			byteArray.write_number(this.int6);
			byteArray.write_number(this.int7);
			byteArray.write_number(this.int8);
			byteArray.write_number(this.float1);
			byteArray.write_number(this.float2);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_number();
			this.int2 = byteArray.read_number();
			this.int3 = byteArray.read_number();
			this.int4 = byteArray.read_number();
			this.int5 = byteArray.read_number();
			this.int6 = byteArray.read_number();
			this.int7 = byteArray.read_number();
			this.int8 = byteArray.read_number();
			this.float1 = byteArray.read_number();
			this.float2 = byteArray.read_number();
	}
}

export class tfloat {
	public static readonly PROTO_ID = 17;
	public msgId: number = 17;
		public int1: number = 0;
		public int2: number = 0;

	public encode(byteArray: ByteArray): void {
			byteArray.write_float(this.int1);
			byteArray.write_float(this.int2);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_float();
			this.int2 = byteArray.read_float();
	}
}

export class tdouble {
	public static readonly PROTO_ID = 18;
	public msgId: number = 18;
		public int1: number = 0;
		public int2: number = 0;

	public encode(byteArray: ByteArray): void {
			byteArray.write_double(this.int1);
			byteArray.write_double(this.int2);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_double();
			this.int2 = byteArray.read_double();
	}
}

export class tstring {
	public static readonly PROTO_ID = 19;
	public msgId: number = 19;
		public int1: string = "";
		public int2: string = "";

	public encode(byteArray: ByteArray): void {
			byteArray.write_string(this.int1);
			byteArray.write_string(this.int2);
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_string();
			this.int2 = byteArray.read_string();
	}
}

export class tlistbool {
	public static readonly PROTO_ID = 20;
	public msgId: number = 20;
		public int1: boolean[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_bool(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_bool());
	}
}

export class tlistint8 {
	public static readonly PROTO_ID = 21;
	public msgId: number = 21;
		public int1: number[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_int8(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_int8());
	}
}

export class tlistuint8 {
	public static readonly PROTO_ID = 22;
	public msgId: number = 22;
		public int1: number[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_uint8(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_uint8());
	}
}

export class tlistint16 {
	public static readonly PROTO_ID = 23;
	public msgId: number = 23;
		public int1: number[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_int16(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_int16());
	}
}

export class tlistuint16 {
	public static readonly PROTO_ID = 24;
	public msgId: number = 24;
		public int1: number[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_uint16(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_uint16());
	}
}

export class tlistint32 {
	public static readonly PROTO_ID = 25;
	public msgId: number = 25;
		public int1: number[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_int32(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_int32());
	}
}

export class tlistuint32 {
	public static readonly PROTO_ID = 26;
	public msgId: number = 26;
		public int1: number[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_uint32(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_uint32());
	}
}

export class tlistint64 {
	public static readonly PROTO_ID = 27;
	public msgId: number = 27;
		public int1: bigint[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_int64(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_int64());
	}
}

export class tlistuint64 {
	public static readonly PROTO_ID = 28;
	public msgId: number = 28;
		public int1: bigint[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_uint64(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_uint64());
	}
}

export class tlistinteger {
	public static readonly PROTO_ID = 29;
	public msgId: number = 29;
		public int1: number[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_integer(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_integer());
	}
}

export class tlistnumber {
	public static readonly PROTO_ID = 30;
	public msgId: number = 30;
		public int1: number[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_number(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_number());
	}
}

export class tlistfloat {
	public static readonly PROTO_ID = 31;
	public msgId: number = 31;
		public int1: number[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_float(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_float());
	}
}

export class tlistdouble {
	public static readonly PROTO_ID = 32;
	public msgId: number = 32;
		public int1: number[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_double(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_double());
	}
}

export class tliststring {
	public static readonly PROTO_ID = 33;
	public msgId: number = 33;
		public int1: string[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => byteArray.write_string(item));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => byteArray.read_string());
	}
}

export class tlistunion {
	public static readonly PROTO_ID = 34;
	public msgId: number = 34;
		public int1: union[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.int1, (item) => item.encode(byteArray));
	}

	public decode(byteArray: ByteArray): void {
			this.int1 = byteArray.read_list(() => {
				const item = new union();
				item.decode(byteArray);
				return item;
			});
	}
}

export class allType {
	public static readonly PROTO_ID = 35;
	public msgId: number = 35;
		public flag: boolean = false;
		public v8: number = 0;
		public uv8: number = 0;
		public v16: number = 0;
		public uv16: number = 0;
		public v32: number = 0;
		public uv32: number = 0;
		public v64: bigint = 0n;
		public uv64: bigint = 0n;
		public inte8: number = 0;
		public uinte8: number = 0;
		public inte16: number = 0;
		public uinte16: number = 0;
		public inte32: number = 0;
		public uinte32: number = 0;
		public inte64: number = 0;
		public uinte64: number = 0;
		public num8: number = 0;
		public unum8: number = 0;
		public num16: number = 0;
		public unum16: number = 0;
		public num32: number = 0;
		public unum32: number = 0;
		public num64: number = 0;
		public unum64: number = 0;
		public numfloat: number = 0;
		public numdouble: number = 0;
		public f32: number = 0;
		public f64: number = 0;
		public string1: string = "";
		public string2: string = "";
		public uval: union | null = null;
		public lbool: boolean[] = [];
		public lint8: number[] = [];
		public luint8: number[] = [];
		public lint16: number[] = [];
		public luint16: number[] = [];
		public lint32: number[] = [];
		public luint32: number[] = [];
		public lint64: bigint[] = [];
		public luint64: bigint[] = [];
		public linte8: number[] = [];
		public linte16: number[] = [];
		public linte32: number[] = [];
		public linte64: number[] = [];
		public lnum8: number[] = [];
		public lnum16: number[] = [];
		public lnum32: number[] = [];
		public lnum64: number[] = [];
		public lnfloat32: number[] = [];
		public lnfloat64: number[] = [];
		public lfloat: number[] = [];
		public ldouble: number[] = [];
		public lstring: string[] = [];
		public lunion: union[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_bool(this.flag);
			byteArray.write_int8(this.v8);
			byteArray.write_uint8(this.uv8);
			byteArray.write_int16(this.v16);
			byteArray.write_uint16(this.uv16);
			byteArray.write_int32(this.v32);
			byteArray.write_uint32(this.uv32);
			byteArray.write_int64(this.v64);
			byteArray.write_uint64(this.uv64);
			byteArray.write_integer(this.inte8);
			byteArray.write_integer(this.uinte8);
			byteArray.write_integer(this.inte16);
			byteArray.write_integer(this.uinte16);
			byteArray.write_integer(this.inte32);
			byteArray.write_integer(this.uinte32);
			byteArray.write_integer(this.inte64);
			byteArray.write_integer(this.uinte64);
			byteArray.write_number(this.num8);
			byteArray.write_number(this.unum8);
			byteArray.write_number(this.num16);
			byteArray.write_number(this.unum16);
			byteArray.write_number(this.num32);
			byteArray.write_number(this.unum32);
			byteArray.write_number(this.num64);
			byteArray.write_number(this.unum64);
			byteArray.write_number(this.numfloat);
			byteArray.write_number(this.numdouble);
			byteArray.write_float(this.f32);
			byteArray.write_double(this.f64);
			byteArray.write_string(this.string1);
			byteArray.write_string(this.string2);
			if (this.uval) {
				byteArray.write_uint8(1);
				this.uval.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			byteArray.write_list(this.lbool, (item) => byteArray.write_bool(item));
			byteArray.write_list(this.lint8, (item) => byteArray.write_int8(item));
			byteArray.write_list(this.luint8, (item) => byteArray.write_uint8(item));
			byteArray.write_list(this.lint16, (item) => byteArray.write_int16(item));
			byteArray.write_list(this.luint16, (item) => byteArray.write_uint16(item));
			byteArray.write_list(this.lint32, (item) => byteArray.write_int32(item));
			byteArray.write_list(this.luint32, (item) => byteArray.write_uint32(item));
			byteArray.write_list(this.lint64, (item) => byteArray.write_int64(item));
			byteArray.write_list(this.luint64, (item) => byteArray.write_uint64(item));
			byteArray.write_list(this.linte8, (item) => byteArray.write_integer(item));
			byteArray.write_list(this.linte16, (item) => byteArray.write_integer(item));
			byteArray.write_list(this.linte32, (item) => byteArray.write_integer(item));
			byteArray.write_list(this.linte64, (item) => byteArray.write_integer(item));
			byteArray.write_list(this.lnum8, (item) => byteArray.write_number(item));
			byteArray.write_list(this.lnum16, (item) => byteArray.write_number(item));
			byteArray.write_list(this.lnum32, (item) => byteArray.write_number(item));
			byteArray.write_list(this.lnum64, (item) => byteArray.write_number(item));
			byteArray.write_list(this.lnfloat32, (item) => byteArray.write_number(item));
			byteArray.write_list(this.lnfloat64, (item) => byteArray.write_number(item));
			byteArray.write_list(this.lfloat, (item) => byteArray.write_float(item));
			byteArray.write_list(this.ldouble, (item) => byteArray.write_double(item));
			byteArray.write_list(this.lstring, (item) => byteArray.write_string(item));
			byteArray.write_list(this.lunion, (item) => item.encode(byteArray));
	}

	public decode(byteArray: ByteArray): void {
			this.flag = byteArray.read_bool();
			this.v8 = byteArray.read_int8();
			this.uv8 = byteArray.read_uint8();
			this.v16 = byteArray.read_int16();
			this.uv16 = byteArray.read_uint16();
			this.v32 = byteArray.read_int32();
			this.uv32 = byteArray.read_uint32();
			this.v64 = byteArray.read_int64();
			this.uv64 = byteArray.read_uint64();
			this.inte8 = byteArray.read_integer();
			this.uinte8 = byteArray.read_integer();
			this.inte16 = byteArray.read_integer();
			this.uinte16 = byteArray.read_integer();
			this.inte32 = byteArray.read_integer();
			this.uinte32 = byteArray.read_integer();
			this.inte64 = byteArray.read_integer();
			this.uinte64 = byteArray.read_integer();
			this.num8 = byteArray.read_number();
			this.unum8 = byteArray.read_number();
			this.num16 = byteArray.read_number();
			this.unum16 = byteArray.read_number();
			this.num32 = byteArray.read_number();
			this.unum32 = byteArray.read_number();
			this.num64 = byteArray.read_number();
			this.unum64 = byteArray.read_number();
			this.numfloat = byteArray.read_number();
			this.numdouble = byteArray.read_number();
			this.f32 = byteArray.read_float();
			this.f64 = byteArray.read_double();
			this.string1 = byteArray.read_string();
			this.string2 = byteArray.read_string();
			const hasuval = byteArray.read_uint8() > 0;
			if (hasuval) {
				this.uval = new union();
				this.uval.decode(byteArray);
			} else {
				this.uval = null;
			}
			this.lbool = byteArray.read_list(() => byteArray.read_bool());
			this.lint8 = byteArray.read_list(() => byteArray.read_int8());
			this.luint8 = byteArray.read_list(() => byteArray.read_uint8());
			this.lint16 = byteArray.read_list(() => byteArray.read_int16());
			this.luint16 = byteArray.read_list(() => byteArray.read_uint16());
			this.lint32 = byteArray.read_list(() => byteArray.read_int32());
			this.luint32 = byteArray.read_list(() => byteArray.read_uint32());
			this.lint64 = byteArray.read_list(() => byteArray.read_int64());
			this.luint64 = byteArray.read_list(() => byteArray.read_uint64());
			this.linte8 = byteArray.read_list(() => byteArray.read_integer());
			this.linte16 = byteArray.read_list(() => byteArray.read_integer());
			this.linte32 = byteArray.read_list(() => byteArray.read_integer());
			this.linte64 = byteArray.read_list(() => byteArray.read_integer());
			this.lnum8 = byteArray.read_list(() => byteArray.read_number());
			this.lnum16 = byteArray.read_list(() => byteArray.read_number());
			this.lnum32 = byteArray.read_list(() => byteArray.read_number());
			this.lnum64 = byteArray.read_list(() => byteArray.read_number());
			this.lnfloat32 = byteArray.read_list(() => byteArray.read_number());
			this.lnfloat64 = byteArray.read_list(() => byteArray.read_number());
			this.lfloat = byteArray.read_list(() => byteArray.read_float());
			this.ldouble = byteArray.read_list(() => byteArray.read_double());
			this.lstring = byteArray.read_list(() => byteArray.read_string());
			this.lunion = byteArray.read_list(() => {
				const item = new union();
				item.decode(byteArray);
				return item;
			});
	}
}

export class testnull {
	public static readonly PROTO_ID = 36;
	public msgId: number = 36;

	public encode(byteArray: ByteArray): void {
			void byteArray;
	}

	public decode(byteArray: ByteArray): void {
			void byteArray;
	}
}

export class tbinary {
	public static readonly PROTO_ID = 37;
	public msgId: number = 37;
		public data: Uint8Array = new Uint8Array();
		public chunks: Uint8Array[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_binary(this.data);
			byteArray.write_list(this.chunks, (item) => byteArray.write_binary(item));
	}

	public decode(byteArray: ByteArray): void {
			this.data = byteArray.read_binary();
			this.chunks = byteArray.read_list(() => byteArray.read_binary());
	}
}

export class tmap {
	public static readonly PROTO_ID = 38;
	public msgId: number = 38;
		public attrs: Map<string, number> | null = null;
		public names: Map<number, string> | null = null;
		public phones: Map<string, phoneNumber> | null = null;

	public encode(byteArray: ByteArray): void {
			byteArray.write_map(this.attrs, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.names, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_string(v);
			});
			byteArray.write_map(this.phones, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				v.encode(byteArray);
			});
	}

	public decode(byteArray: ByteArray): void {
			this.attrs = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.names = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
			this.phones = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = new phoneNumber();
				__v.decode(byteArray);
				return __v;
			});
	}
}

export class toneof {
	public static readonly PROTO_ID = 39;
	public msgId: number = 39;
		public reason: number = 0;
		public item: [number, any] | null = null;
		public item2: [number, any] | null = null;
		public tip: string = "";

	public encode(byteArray: ByteArray): void {
			byteArray.write_int32(this.reason);
			if (!this.item) {
				byteArray.write_uint8(0);
			} else {
				switch (this.item[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.item[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.item[1]);
						break;
					case 3:
						byteArray.write_uint8(3);
						this.item[1].encode(byteArray);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			if (!this.item2) {
				byteArray.write_uint8(0);
			} else {
				switch (this.item2[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.item2[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.item2[1]);
						break;
					case 3:
						byteArray.write_uint8(3);
						this.item2[1].encode(byteArray);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_string(this.tip);
	}

	public decode(byteArray: ByteArray): void {
			this.reason = byteArray.read_int32();
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.item = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.item = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.item = [2, __ov];
						break;
					}
					case 3:
					{
						let __ov: any;
						__ov = new phoneNumber();
						__ov.decode(byteArray);
						this.item = [3, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.item2 = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.item2 = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.item2 = [2, __ov];
						break;
					}
					case 3:
					{
						let __ov: any;
						__ov = new phoneNumber();
						__ov.decode(byteArray);
						this.item2 = [3, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.tip = byteArray.read_string();
	}
}

export class person1 {
	public static readonly PROTO_ID = 1001;
	public msgId: number = 1001;
		public name: string = "";
		public id: number = 0;
		public email: string = "";
		public phone: phoneNumber[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_string(this.name);
			byteArray.write_int32(this.id);
			byteArray.write_string(this.email);
			byteArray.write_list(this.phone, (item) => item.encode(byteArray));
	}

	public decode(byteArray: ByteArray): void {
			this.name = byteArray.read_string();
			this.id = byteArray.read_int32();
			this.email = byteArray.read_string();
			this.phone = byteArray.read_list(() => {
				const item = new phoneNumber();
				item.decode(byteArray);
				return item;
			});
	}
}

export class comboLeaf {
	public static readonly PROTO_ID = 2001;
	public msgId: number = 2001;
		public id: number = 0;
		public tag: string = "";

	public encode(byteArray: ByteArray): void {
			byteArray.write_int32(this.id);
			byteArray.write_string(this.tag);
	}

	public decode(byteArray: ByteArray): void {
			this.id = byteArray.read_int32();
			this.tag = byteArray.read_string();
	}
}

export class comboNode {
	public static readonly PROTO_ID = 2002;
	public msgId: number = 2002;
		public head: comboLeaf | null = null;
		public leaves: comboLeaf[] = [];
		public index: Map<string, number> | null = null;

	public encode(byteArray: ByteArray): void {
			if (this.head) {
				byteArray.write_uint8(1);
				this.head.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			byteArray.write_list(this.leaves, (item) => item.encode(byteArray));
			byteArray.write_map(this.index, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
	}

	public decode(byteArray: ByteArray): void {
			const hashead = byteArray.read_uint8() > 0;
			if (hashead) {
				this.head = new comboLeaf();
				this.head.decode(byteArray);
			} else {
				this.head = null;
			}
			this.leaves = byteArray.read_list(() => {
				const item = new comboLeaf();
				item.decode(byteArray);
				return item;
			});
			this.index = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
	}
}

export class comboWrap {
	public static readonly PROTO_ID = 2003;
	public msgId: number = 2003;
		public name: string = "";
		public node: comboNode | null = null;
		public rev: Map<number, string> | null = null;

	public encode(byteArray: ByteArray): void {
			byteArray.write_string(this.name);
			if (this.node) {
				byteArray.write_uint8(1);
				this.node.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			byteArray.write_map(this.rev, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_string(v);
			});
	}

	public decode(byteArray: ByteArray): void {
			this.name = byteArray.read_string();
			const hasnode = byteArray.read_uint8() > 0;
			if (hasnode) {
				this.node = new comboNode();
				this.node.decode(byteArray);
			} else {
				this.node = null;
			}
			this.rev = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
	}
}

export class tMapMatrix {
	public static readonly PROTO_ID = 2004;
	public msgId: number = 2004;
		public s_i: Map<string, number> | null = null;
		public i_s: Map<number, string> | null = null;
		public s_s: Map<string, string> | null = null;
		public i_i: Map<number, number> | null = null;
		public i64_i: Map<bigint, number> | null = null;
		public u_s: Map<number, string> | null = null;
		public s_b: Map<string, Uint8Array> | null = null;
		public i_b: Map<number, Uint8Array> | null = null;
		public s_rec: Map<string, phoneNumber> | null = null;
		public i_leaf: Map<number, comboLeaf> | null = null;

	public encode(byteArray: ByteArray): void {
			byteArray.write_map(this.s_i, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.i_s, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_string(v);
			});
			byteArray.write_map(this.s_s, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_string(v);
			});
			byteArray.write_map(this.i_i, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.i64_i, (k) => {
				byteArray.write_int64(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.u_s, (k) => {
				byteArray.write_uint32(k);
			}, (v) => {
				byteArray.write_string(v);
			});
			byteArray.write_map(this.s_b, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_binary(v);
			});
			byteArray.write_map(this.i_b, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_binary(v);
			});
			byteArray.write_map(this.s_rec, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				v.encode(byteArray);
			});
			byteArray.write_map(this.i_leaf, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				v.encode(byteArray);
			});
	}

	public decode(byteArray: ByteArray): void {
			this.s_i = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.i_s = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
			this.s_s = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
			this.i_i = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.i64_i = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int64();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.u_s = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_uint32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
			this.s_b = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_binary();
				return __v;
			});
			this.i_b = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_binary();
				return __v;
			});
			this.s_rec = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = new phoneNumber();
				__v.decode(byteArray);
				return __v;
			});
			this.i_leaf = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = new comboLeaf();
				__v.decode(byteArray);
				return __v;
			});
	}
}

export class tListMatrix {
	public static readonly PROTO_ID = 2005;
	public msgId: number = 2005;
		public lb: boolean[] = [];
		public li8: number[] = [];
		public lu8: number[] = [];
		public li16: number[] = [];
		public lu16: number[] = [];
		public li32: number[] = [];
		public lu32: number[] = [];
		public li64: bigint[] = [];
		public lu64: bigint[] = [];
		public lint: number[] = [];
		public lnum: number[] = [];
		public lflt: number[] = [];
		public ldbl: number[] = [];
		public lstr: string[] = [];
		public lbin: Uint8Array[] = [];
		public luni: union[] = [];
		public lph: phoneNumber[] = [];
		public lleaf: comboLeaf[] = [];
		public lnode: comboNode[] = [];
		public lli32_1: number[] = [];
		public lli32_2: number[] = [];
		public lli32_3: number[] = [];
		public lmap_1: Map<string, number> | null = null;
		public lmap_2: Map<string, number> | null = null;
		public lmap_3: Map<string, number> | null = null;

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.lb, (item) => byteArray.write_bool(item));
			byteArray.write_list(this.li8, (item) => byteArray.write_int8(item));
			byteArray.write_list(this.lu8, (item) => byteArray.write_uint8(item));
			byteArray.write_list(this.li16, (item) => byteArray.write_int16(item));
			byteArray.write_list(this.lu16, (item) => byteArray.write_uint16(item));
			byteArray.write_list(this.li32, (item) => byteArray.write_int32(item));
			byteArray.write_list(this.lu32, (item) => byteArray.write_uint32(item));
			byteArray.write_list(this.li64, (item) => byteArray.write_int64(item));
			byteArray.write_list(this.lu64, (item) => byteArray.write_uint64(item));
			byteArray.write_list(this.lint, (item) => byteArray.write_integer(item));
			byteArray.write_list(this.lnum, (item) => byteArray.write_number(item));
			byteArray.write_list(this.lflt, (item) => byteArray.write_float(item));
			byteArray.write_list(this.ldbl, (item) => byteArray.write_double(item));
			byteArray.write_list(this.lstr, (item) => byteArray.write_string(item));
			byteArray.write_list(this.lbin, (item) => byteArray.write_binary(item));
			byteArray.write_list(this.luni, (item) => item.encode(byteArray));
			byteArray.write_list(this.lph, (item) => item.encode(byteArray));
			byteArray.write_list(this.lleaf, (item) => item.encode(byteArray));
			byteArray.write_list(this.lnode, (item) => item.encode(byteArray));
			byteArray.write_list(this.lli32_1, (item) => byteArray.write_int32(item));
			byteArray.write_list(this.lli32_2, (item) => byteArray.write_int32(item));
			byteArray.write_list(this.lli32_3, (item) => byteArray.write_int32(item));
			byteArray.write_map(this.lmap_1, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.lmap_2, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.lmap_3, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
	}

	public decode(byteArray: ByteArray): void {
			this.lb = byteArray.read_list(() => byteArray.read_bool());
			this.li8 = byteArray.read_list(() => byteArray.read_int8());
			this.lu8 = byteArray.read_list(() => byteArray.read_uint8());
			this.li16 = byteArray.read_list(() => byteArray.read_int16());
			this.lu16 = byteArray.read_list(() => byteArray.read_uint16());
			this.li32 = byteArray.read_list(() => byteArray.read_int32());
			this.lu32 = byteArray.read_list(() => byteArray.read_uint32());
			this.li64 = byteArray.read_list(() => byteArray.read_int64());
			this.lu64 = byteArray.read_list(() => byteArray.read_uint64());
			this.lint = byteArray.read_list(() => byteArray.read_integer());
			this.lnum = byteArray.read_list(() => byteArray.read_number());
			this.lflt = byteArray.read_list(() => byteArray.read_float());
			this.ldbl = byteArray.read_list(() => byteArray.read_double());
			this.lstr = byteArray.read_list(() => byteArray.read_string());
			this.lbin = byteArray.read_list(() => byteArray.read_binary());
			this.luni = byteArray.read_list(() => {
				const item = new union();
				item.decode(byteArray);
				return item;
			});
			this.lph = byteArray.read_list(() => {
				const item = new phoneNumber();
				item.decode(byteArray);
				return item;
			});
			this.lleaf = byteArray.read_list(() => {
				const item = new comboLeaf();
				item.decode(byteArray);
				return item;
			});
			this.lnode = byteArray.read_list(() => {
				const item = new comboNode();
				item.decode(byteArray);
				return item;
			});
			this.lli32_1 = byteArray.read_list(() => byteArray.read_int32());
			this.lli32_2 = byteArray.read_list(() => byteArray.read_int32());
			this.lli32_3 = byteArray.read_list(() => byteArray.read_int32());
			this.lmap_1 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.lmap_2 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.lmap_3 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
	}
}

export class tOneofMatrix {
	public static readonly PROTO_ID = 2006;
	public msgId: number = 2006;
		public payload: [number, any] | null = null;

	public encode(byteArray: ByteArray): void {
			if (!this.payload) {
				byteArray.write_uint8(0);
			} else {
				switch (this.payload[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_bool(this.payload[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_int32(this.payload[1]);
						break;
					case 3:
						byteArray.write_uint8(3);
						byteArray.write_string(this.payload[1]);
						break;
					case 4:
						byteArray.write_uint8(4);
						byteArray.write_binary(this.payload[1]);
						break;
					case 5:
						byteArray.write_uint8(5);
						this.payload[1].encode(byteArray);
						break;
					case 6:
						byteArray.write_uint8(6);
						this.payload[1].encode(byteArray);
						break;
					case 7:
						byteArray.write_uint8(7);
						this.payload[1].encode(byteArray);
						break;
					case 8:
						byteArray.write_uint8(8);
						byteArray.write_list(this.payload[1], (item) => byteArray.write_int32(item));
						break;
					case 9:
						byteArray.write_uint8(9);
						byteArray.write_list(this.payload[1], (item) => byteArray.write_string(item));
						break;
					default: throw new Error('invalid oneof');
				}
			}
	}

	public decode(byteArray: ByteArray): void {
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.payload = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_bool();
						this.payload = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.payload = [2, __ov];
						break;
					}
					case 3:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.payload = [3, __ov];
						break;
					}
					case 4:
					{
						let __ov: any;
						__ov = byteArray.read_binary();
						this.payload = [4, __ov];
						break;
					}
					case 5:
					{
						let __ov: any;
						__ov = new phoneNumber();
						__ov.decode(byteArray);
						this.payload = [5, __ov];
						break;
					}
					case 6:
					{
						let __ov: any;
						__ov = new comboLeaf();
						__ov.decode(byteArray);
						this.payload = [6, __ov];
						break;
					}
					case 7:
					{
						let __ov: any;
						__ov = new comboNode();
						__ov.decode(byteArray);
						this.payload = [7, __ov];
						break;
					}
					case 8:
					{
						let __ov: any;
						__ov = byteArray.read_list(() => byteArray.read_int32());
						this.payload = [8, __ov];
						break;
					}
					case 9:
					{
						let __ov: any;
						__ov = byteArray.read_list(() => byteArray.read_string());
						this.payload = [9, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
	}
}

export class tNestDeep {
	public static readonly PROTO_ID = 2007;
	public msgId: number = 2007;
		public ms_leaf: Map<string, comboLeaf> | null = null;
		public wrap: comboWrap | null = null;
		public nested: [number, any] | null = null;
		public lwrap_1: comboWrap[] = [];
		public lwrap_2: comboWrap[] = [];
		public lwrap_3: comboWrap[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_map(this.ms_leaf, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				v.encode(byteArray);
			});
			if (this.wrap) {
				byteArray.write_uint8(1);
				this.wrap.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (!this.nested) {
				byteArray.write_uint8(0);
			} else {
				switch (this.nested[0]) {
					case 1:
						byteArray.write_uint8(1);
						this.nested[1].encode(byteArray);
						break;
					case 2:
						byteArray.write_uint8(2);
						this.nested[1].encode(byteArray);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_list(this.lwrap_1, (item) => item.encode(byteArray));
			byteArray.write_list(this.lwrap_2, (item) => item.encode(byteArray));
			byteArray.write_list(this.lwrap_3, (item) => item.encode(byteArray));
	}

	public decode(byteArray: ByteArray): void {
			this.ms_leaf = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = new comboLeaf();
				__v.decode(byteArray);
				return __v;
			});
			const haswrap = byteArray.read_uint8() > 0;
			if (haswrap) {
				this.wrap = new comboWrap();
				this.wrap.decode(byteArray);
			} else {
				this.wrap = null;
			}
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.nested = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = new comboNode();
						__ov.decode(byteArray);
						this.nested = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = new comboLeaf();
						__ov.decode(byteArray);
						this.nested = [2, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.lwrap_1 = byteArray.read_list(() => {
				const item = new comboWrap();
				item.decode(byteArray);
				return item;
			});
			this.lwrap_2 = byteArray.read_list(() => {
				const item = new comboWrap();
				item.decode(byteArray);
				return item;
			});
			this.lwrap_3 = byteArray.read_list(() => {
				const item = new comboWrap();
				item.decode(byteArray);
				return item;
			});
	}
}

export class tInterleave {
	public static readonly PROTO_ID = 2008;
	public msgId: number = 2008;
		public n1: number = 0;
		public s1: string = "";
		public m1: Map<string, number> | null = null;
		public l1: number[] = [];
		public o1: [number, any] | null = null;
		public n2: number = 0;
		public s2: string = "";
		public m2: Map<string, number> | null = null;
		public l2: number[] = [];
		public o2: [number, any] | null = null;
		public n3: number = 0;
		public s3: string = "";
		public m3: Map<string, number> | null = null;
		public l3: number[] = [];
		public o3: [number, any] | null = null;
		public b1: Uint8Array = new Uint8Array();
		public b2: Uint8Array = new Uint8Array();
		public b3: Uint8Array = new Uint8Array();

	public encode(byteArray: ByteArray): void {
			byteArray.write_int32(this.n1);
			byteArray.write_string(this.s1);
			byteArray.write_map(this.m1, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_list(this.l1, (item) => byteArray.write_int32(item));
			if (!this.o1) {
				byteArray.write_uint8(0);
			} else {
				switch (this.o1[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.o1[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.o1[1]);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_int32(this.n2);
			byteArray.write_string(this.s2);
			byteArray.write_map(this.m2, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_list(this.l2, (item) => byteArray.write_int32(item));
			if (!this.o2) {
				byteArray.write_uint8(0);
			} else {
				switch (this.o2[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.o2[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.o2[1]);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_int32(this.n3);
			byteArray.write_string(this.s3);
			byteArray.write_map(this.m3, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_list(this.l3, (item) => byteArray.write_int32(item));
			if (!this.o3) {
				byteArray.write_uint8(0);
			} else {
				switch (this.o3[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.o3[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.o3[1]);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_binary(this.b1);
			byteArray.write_binary(this.b2);
			byteArray.write_binary(this.b3);
	}

	public decode(byteArray: ByteArray): void {
			this.n1 = byteArray.read_int32();
			this.s1 = byteArray.read_string();
			this.m1 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.l1 = byteArray.read_list(() => byteArray.read_int32());
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.o1 = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.o1 = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.o1 = [2, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.n2 = byteArray.read_int32();
			this.s2 = byteArray.read_string();
			this.m2 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.l2 = byteArray.read_list(() => byteArray.read_int32());
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.o2 = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.o2 = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.o2 = [2, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.n3 = byteArray.read_int32();
			this.s3 = byteArray.read_string();
			this.m3 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.l3 = byteArray.read_list(() => byteArray.read_int32());
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.o3 = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.o3 = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.o3 = [2, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.b1 = byteArray.read_binary();
			this.b2 = byteArray.read_binary();
			this.b3 = byteArray.read_binary();
	}
}

export class tMultiOneof {
	public static readonly PROTO_ID = 2009;
	public msgId: number = 2009;
		public a: [number, any] | null = null;
		public g1: number = 0;
		public b: [number, any] | null = null;
		public g2: number = 0;
		public c: [number, any] | null = null;
		public g3: number = 0;
		public d: [number, any] | null = null;
		public g4: number = 0;
		public e: [number, any] | null = null;

	public encode(byteArray: ByteArray): void {
			if (!this.a) {
				byteArray.write_uint8(0);
			} else {
				switch (this.a[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.a[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.a[1]);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_int32(this.g1);
			if (!this.b) {
				byteArray.write_uint8(0);
			} else {
				switch (this.b[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.b[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.b[1]);
						break;
					case 3:
						byteArray.write_uint8(3);
						byteArray.write_binary(this.b[1]);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_int32(this.g2);
			if (!this.c) {
				byteArray.write_uint8(0);
			} else {
				switch (this.c[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.c[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						this.c[1].encode(byteArray);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_int32(this.g3);
			if (!this.d) {
				byteArray.write_uint8(0);
			} else {
				switch (this.d[0]) {
					case 1:
						byteArray.write_uint8(1);
						this.d[1].encode(byteArray);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_list(this.d[1], (item) => byteArray.write_int32(item));
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_int32(this.g4);
			if (!this.e) {
				byteArray.write_uint8(0);
			} else {
				switch (this.e[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_string(this.e[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_int32(this.e[1]);
						break;
					default: throw new Error('invalid oneof');
				}
			}
	}

	public decode(byteArray: ByteArray): void {
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.a = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.a = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.a = [2, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.g1 = byteArray.read_int32();
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.b = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.b = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.b = [2, __ov];
						break;
					}
					case 3:
					{
						let __ov: any;
						__ov = byteArray.read_binary();
						this.b = [3, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.g2 = byteArray.read_int32();
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.c = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.c = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = new phoneNumber();
						__ov.decode(byteArray);
						this.c = [2, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.g3 = byteArray.read_int32();
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.d = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = new comboLeaf();
						__ov.decode(byteArray);
						this.d = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_list(() => byteArray.read_int32());
						this.d = [2, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.g4 = byteArray.read_int32();
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.e = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.e = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.e = [2, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
	}
}

export class tEmptyEdge {
	public static readonly PROTO_ID = 2010;
	public msgId: number = 2010;
		public empty_map: Map<string, number> | null = null;
		public empty_list: number[] = [];
		public empty_strs: string[] = [];
		public empty_bin: Uint8Array = new Uint8Array();
		public empty_str: string = "";
		public none: [number, any] | null = null;
		public empty_phone: phoneNumber | null = null;
		public empty_leaf: comboLeaf | null = null;

	public encode(byteArray: ByteArray): void {
			byteArray.write_map(this.empty_map, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_list(this.empty_list, (item) => byteArray.write_int32(item));
			byteArray.write_list(this.empty_strs, (item) => byteArray.write_string(item));
			byteArray.write_binary(this.empty_bin);
			byteArray.write_string(this.empty_str);
			if (!this.none) {
				byteArray.write_uint8(0);
			} else {
				switch (this.none[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.none[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.none[1]);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			if (this.empty_phone) {
				byteArray.write_uint8(1);
				this.empty_phone.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.empty_leaf) {
				byteArray.write_uint8(1);
				this.empty_leaf.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
	}

	public decode(byteArray: ByteArray): void {
			this.empty_map = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.empty_list = byteArray.read_list(() => byteArray.read_int32());
			this.empty_strs = byteArray.read_list(() => byteArray.read_string());
			this.empty_bin = byteArray.read_binary();
			this.empty_str = byteArray.read_string();
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.none = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.none = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.none = [2, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			const hasempty_phone = byteArray.read_uint8() > 0;
			if (hasempty_phone) {
				this.empty_phone = new phoneNumber();
				this.empty_phone.decode(byteArray);
			} else {
				this.empty_phone = null;
			}
			const hasempty_leaf = byteArray.read_uint8() > 0;
			if (hasempty_leaf) {
				this.empty_leaf = new comboLeaf();
				this.empty_leaf.decode(byteArray);
			} else {
				this.empty_leaf = null;
			}
	}
}

export class tScalarRun {
	public static readonly PROTO_ID = 2011;
	public msgId: number = 2011;
		public a1: number = 0;
		public a2: number = 0;
		public a3: number = 0;
		public b1: string = "";
		public b2: string = "";
		public b3: string = "";
		public c1: Map<string, number> | null = null;
		public c2: Map<string, number> | null = null;
		public c3: Map<string, number> | null = null;

	public encode(byteArray: ByteArray): void {
			byteArray.write_int32(this.a1);
			byteArray.write_int32(this.a2);
			byteArray.write_int32(this.a3);
			byteArray.write_string(this.b1);
			byteArray.write_string(this.b2);
			byteArray.write_string(this.b3);
			byteArray.write_map(this.c1, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.c2, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.c3, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
	}

	public decode(byteArray: ByteArray): void {
			this.a1 = byteArray.read_int32();
			this.a2 = byteArray.read_int32();
			this.a3 = byteArray.read_int32();
			this.b1 = byteArray.read_string();
			this.b2 = byteArray.read_string();
			this.b3 = byteArray.read_string();
			this.c1 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.c2 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.c3 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
	}
}

export class tRepeatAll {
	public static readonly PROTO_ID = 2012;
	public msgId: number = 2012;
		public b1: boolean = false;
		public b2: boolean = false;
		public b3: boolean = false;
		public i32_1: number = 0;
		public i32_2: number = 0;
		public i32_3: number = 0;
		public gap_a: string = "";
		public i32_4: number = 0;
		public i8_1: number = 0;
		public i8_2: number = 0;
		public u8_1: number = 0;
		public i8_3: number = 0;
		public u8_2: number = 0;
		public u8_3: number = 0;
		public i16_1: number = 0;
		public i16_2: number = 0;
		public i16_3: number = 0;
		public u16_1: number = 0;
		public gap_b: string = "";
		public u16_2: number = 0;
		public u16_3: number = 0;
		public i32_5: number = 0;
		public i32_6: number = 0;
		public i32_7: number = 0;
		public u32_1: number = 0;
		public u32_2: number = 0;
		public u32_3: number = 0;
		public i64_1: bigint = 0n;
		public i64_2: bigint = 0n;
		public gap_c: string = "";
		public i64_3: bigint = 0n;
		public u64_1: bigint = 0n;
		public u64_2: bigint = 0n;
		public u64_3: bigint = 0n;
		public inte_1: number = 0;
		public inte_2: number = 0;
		public inte_3: number = 0;
		public num_1: number = 0;
		public gap_d: string = "";
		public num_2: number = 0;
		public num_3: number = 0;
		public flt_1: number = 0;
		public flt_2: number = 0;
		public flt_3: number = 0;
		public dbl_1: number = 0;
		public dbl_2: number = 0;
		public dbl_3: number = 0;
		public str_1: string = "";
		public str_2: string = "";
		public str_3: string = "";
		public gap_e: number = 0;
		public str_4: string = "";
		public bin_1: Uint8Array = new Uint8Array();
		public bin_2: Uint8Array = new Uint8Array();
		public bin_3: Uint8Array = new Uint8Array();
		public uni_1: union | null = null;
		public uni_2: union | null = null;
		public uni_3: union | null = null;
		public li32_1: number[] = [];
		public li32_2: number[] = [];
		public li32_3: number[] = [];
		public lstr_1: string[] = [];
		public gap_f: number = 0;
		public lstr_2: string[] = [];
		public lstr_3: string[] = [];
		public lbin_1: Uint8Array[] = [];
		public lbin_2: Uint8Array[] = [];
		public lbin_3: Uint8Array[] = [];
		public lbool_1: boolean[] = [];
		public lbool_2: boolean[] = [];
		public lbool_3: boolean[] = [];
		public lint_1: number[] = [];
		public lint_2: number[] = [];
		public lint_3: number[] = [];
		public lnum_1: number[] = [];
		public lnum_2: number[] = [];
		public lnum_3: number[] = [];
		public lph_1: phoneNumber[] = [];
		public lph_2: phoneNumber[] = [];
		public gap_g: string = "";
		public lph_3: phoneNumber[] = [];
		public lleaf_1: comboLeaf[] = [];
		public lleaf_2: comboLeaf[] = [];
		public lleaf_3: comboLeaf[] = [];
		public ms_i_1: Map<string, number> | null = null;
		public ms_i_2: Map<string, number> | null = null;
		public ms_i_3: Map<string, number> | null = null;
		public mi_s_1: Map<number, string> | null = null;
		public gap_h: number = 0;
		public mi_s_2: Map<number, string> | null = null;
		public mi_s_3: Map<number, string> | null = null;
		public ms_s_1: Map<string, string> | null = null;
		public ms_s_2: Map<string, string> | null = null;
		public ms_s_3: Map<string, string> | null = null;
		public mi_i_1: Map<number, number> | null = null;
		public mi_i_2: Map<number, number> | null = null;
		public mi_i_3: Map<number, number> | null = null;
		public ms_b_1: Map<string, Uint8Array> | null = null;
		public ms_b_2: Map<string, Uint8Array> | null = null;
		public ms_b_3: Map<string, Uint8Array> | null = null;
		public mi_b_1: Map<number, Uint8Array> | null = null;
		public mi_b_2: Map<number, Uint8Array> | null = null;
		public mi_b_3: Map<number, Uint8Array> | null = null;
		public ms_p_1: Map<string, phoneNumber> | null = null;
		public ms_p_2: Map<string, phoneNumber> | null = null;
		public ms_p_3: Map<string, phoneNumber> | null = null;
		public mi_l_1: Map<number, comboLeaf> | null = null;
		public mi_l_2: Map<number, comboLeaf> | null = null;
		public mi_l_3: Map<number, comboLeaf> | null = null;
		public pick1: [number, any] | null = null;
		public gap_i: number = 0;
		public pick2: [number, any] | null = null;
		public gap_j: string = "";
		public pick3: [number, any] | null = null;
		public ph_1: phoneNumber | null = null;
		public ph_2: phoneNumber | null = null;
		public gap_k: string = "";
		public ph_3: phoneNumber | null = null;
		public leaf_1: comboLeaf | null = null;
		public leaf_2: comboLeaf | null = null;
		public leaf_3: comboLeaf | null = null;
		public node_1: comboNode | null = null;
		public node_2: comboNode | null = null;
		public node_3: comboNode | null = null;
		public wrap_1: comboWrap | null = null;
		public wrap_2: comboWrap | null = null;
		public wrap_3: comboWrap | null = null;

	public encode(byteArray: ByteArray): void {
			byteArray.write_bool(this.b1);
			byteArray.write_bool(this.b2);
			byteArray.write_bool(this.b3);
			byteArray.write_int32(this.i32_1);
			byteArray.write_int32(this.i32_2);
			byteArray.write_int32(this.i32_3);
			byteArray.write_string(this.gap_a);
			byteArray.write_int32(this.i32_4);
			byteArray.write_int8(this.i8_1);
			byteArray.write_int8(this.i8_2);
			byteArray.write_uint8(this.u8_1);
			byteArray.write_int8(this.i8_3);
			byteArray.write_uint8(this.u8_2);
			byteArray.write_uint8(this.u8_3);
			byteArray.write_int16(this.i16_1);
			byteArray.write_int16(this.i16_2);
			byteArray.write_int16(this.i16_3);
			byteArray.write_uint16(this.u16_1);
			byteArray.write_string(this.gap_b);
			byteArray.write_uint16(this.u16_2);
			byteArray.write_uint16(this.u16_3);
			byteArray.write_int32(this.i32_5);
			byteArray.write_int32(this.i32_6);
			byteArray.write_int32(this.i32_7);
			byteArray.write_uint32(this.u32_1);
			byteArray.write_uint32(this.u32_2);
			byteArray.write_uint32(this.u32_3);
			byteArray.write_int64(this.i64_1);
			byteArray.write_int64(this.i64_2);
			byteArray.write_string(this.gap_c);
			byteArray.write_int64(this.i64_3);
			byteArray.write_uint64(this.u64_1);
			byteArray.write_uint64(this.u64_2);
			byteArray.write_uint64(this.u64_3);
			byteArray.write_integer(this.inte_1);
			byteArray.write_integer(this.inte_2);
			byteArray.write_integer(this.inte_3);
			byteArray.write_number(this.num_1);
			byteArray.write_string(this.gap_d);
			byteArray.write_number(this.num_2);
			byteArray.write_number(this.num_3);
			byteArray.write_float(this.flt_1);
			byteArray.write_float(this.flt_2);
			byteArray.write_float(this.flt_3);
			byteArray.write_double(this.dbl_1);
			byteArray.write_double(this.dbl_2);
			byteArray.write_double(this.dbl_3);
			byteArray.write_string(this.str_1);
			byteArray.write_string(this.str_2);
			byteArray.write_string(this.str_3);
			byteArray.write_int32(this.gap_e);
			byteArray.write_string(this.str_4);
			byteArray.write_binary(this.bin_1);
			byteArray.write_binary(this.bin_2);
			byteArray.write_binary(this.bin_3);
			if (this.uni_1) {
				byteArray.write_uint8(1);
				this.uni_1.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.uni_2) {
				byteArray.write_uint8(1);
				this.uni_2.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.uni_3) {
				byteArray.write_uint8(1);
				this.uni_3.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			byteArray.write_list(this.li32_1, (item) => byteArray.write_int32(item));
			byteArray.write_list(this.li32_2, (item) => byteArray.write_int32(item));
			byteArray.write_list(this.li32_3, (item) => byteArray.write_int32(item));
			byteArray.write_list(this.lstr_1, (item) => byteArray.write_string(item));
			byteArray.write_int32(this.gap_f);
			byteArray.write_list(this.lstr_2, (item) => byteArray.write_string(item));
			byteArray.write_list(this.lstr_3, (item) => byteArray.write_string(item));
			byteArray.write_list(this.lbin_1, (item) => byteArray.write_binary(item));
			byteArray.write_list(this.lbin_2, (item) => byteArray.write_binary(item));
			byteArray.write_list(this.lbin_3, (item) => byteArray.write_binary(item));
			byteArray.write_list(this.lbool_1, (item) => byteArray.write_bool(item));
			byteArray.write_list(this.lbool_2, (item) => byteArray.write_bool(item));
			byteArray.write_list(this.lbool_3, (item) => byteArray.write_bool(item));
			byteArray.write_list(this.lint_1, (item) => byteArray.write_integer(item));
			byteArray.write_list(this.lint_2, (item) => byteArray.write_integer(item));
			byteArray.write_list(this.lint_3, (item) => byteArray.write_integer(item));
			byteArray.write_list(this.lnum_1, (item) => byteArray.write_number(item));
			byteArray.write_list(this.lnum_2, (item) => byteArray.write_number(item));
			byteArray.write_list(this.lnum_3, (item) => byteArray.write_number(item));
			byteArray.write_list(this.lph_1, (item) => item.encode(byteArray));
			byteArray.write_list(this.lph_2, (item) => item.encode(byteArray));
			byteArray.write_string(this.gap_g);
			byteArray.write_list(this.lph_3, (item) => item.encode(byteArray));
			byteArray.write_list(this.lleaf_1, (item) => item.encode(byteArray));
			byteArray.write_list(this.lleaf_2, (item) => item.encode(byteArray));
			byteArray.write_list(this.lleaf_3, (item) => item.encode(byteArray));
			byteArray.write_map(this.ms_i_1, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.ms_i_2, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.ms_i_3, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.mi_s_1, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_string(v);
			});
			byteArray.write_int32(this.gap_h);
			byteArray.write_map(this.mi_s_2, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_string(v);
			});
			byteArray.write_map(this.mi_s_3, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_string(v);
			});
			byteArray.write_map(this.ms_s_1, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_string(v);
			});
			byteArray.write_map(this.ms_s_2, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_string(v);
			});
			byteArray.write_map(this.ms_s_3, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_string(v);
			});
			byteArray.write_map(this.mi_i_1, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.mi_i_2, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.mi_i_3, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.ms_b_1, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_binary(v);
			});
			byteArray.write_map(this.ms_b_2, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_binary(v);
			});
			byteArray.write_map(this.ms_b_3, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_binary(v);
			});
			byteArray.write_map(this.mi_b_1, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_binary(v);
			});
			byteArray.write_map(this.mi_b_2, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_binary(v);
			});
			byteArray.write_map(this.mi_b_3, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_binary(v);
			});
			byteArray.write_map(this.ms_p_1, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				v.encode(byteArray);
			});
			byteArray.write_map(this.ms_p_2, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				v.encode(byteArray);
			});
			byteArray.write_map(this.ms_p_3, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				v.encode(byteArray);
			});
			byteArray.write_map(this.mi_l_1, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				v.encode(byteArray);
			});
			byteArray.write_map(this.mi_l_2, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				v.encode(byteArray);
			});
			byteArray.write_map(this.mi_l_3, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				v.encode(byteArray);
			});
			if (!this.pick1) {
				byteArray.write_uint8(0);
			} else {
				switch (this.pick1[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.pick1[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.pick1[1]);
						break;
					case 3:
						byteArray.write_uint8(3);
						byteArray.write_binary(this.pick1[1]);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_int32(this.gap_i);
			if (!this.pick2) {
				byteArray.write_uint8(0);
			} else {
				switch (this.pick2[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.pick2[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.pick2[1]);
						break;
					case 3:
						byteArray.write_uint8(3);
						this.pick2[1].encode(byteArray);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_string(this.gap_j);
			if (!this.pick3) {
				byteArray.write_uint8(0);
			} else {
				switch (this.pick3[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.pick3[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						this.pick3[1].encode(byteArray);
						break;
					case 3:
						byteArray.write_uint8(3);
						byteArray.write_list(this.pick3[1], (item) => byteArray.write_int32(item));
						break;
					default: throw new Error('invalid oneof');
				}
			}
			if (this.ph_1) {
				byteArray.write_uint8(1);
				this.ph_1.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.ph_2) {
				byteArray.write_uint8(1);
				this.ph_2.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			byteArray.write_string(this.gap_k);
			if (this.ph_3) {
				byteArray.write_uint8(1);
				this.ph_3.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.leaf_1) {
				byteArray.write_uint8(1);
				this.leaf_1.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.leaf_2) {
				byteArray.write_uint8(1);
				this.leaf_2.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.leaf_3) {
				byteArray.write_uint8(1);
				this.leaf_3.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.node_1) {
				byteArray.write_uint8(1);
				this.node_1.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.node_2) {
				byteArray.write_uint8(1);
				this.node_2.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.node_3) {
				byteArray.write_uint8(1);
				this.node_3.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.wrap_1) {
				byteArray.write_uint8(1);
				this.wrap_1.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.wrap_2) {
				byteArray.write_uint8(1);
				this.wrap_2.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.wrap_3) {
				byteArray.write_uint8(1);
				this.wrap_3.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
	}

	public decode(byteArray: ByteArray): void {
			this.b1 = byteArray.read_bool();
			this.b2 = byteArray.read_bool();
			this.b3 = byteArray.read_bool();
			this.i32_1 = byteArray.read_int32();
			this.i32_2 = byteArray.read_int32();
			this.i32_3 = byteArray.read_int32();
			this.gap_a = byteArray.read_string();
			this.i32_4 = byteArray.read_int32();
			this.i8_1 = byteArray.read_int8();
			this.i8_2 = byteArray.read_int8();
			this.u8_1 = byteArray.read_uint8();
			this.i8_3 = byteArray.read_int8();
			this.u8_2 = byteArray.read_uint8();
			this.u8_3 = byteArray.read_uint8();
			this.i16_1 = byteArray.read_int16();
			this.i16_2 = byteArray.read_int16();
			this.i16_3 = byteArray.read_int16();
			this.u16_1 = byteArray.read_uint16();
			this.gap_b = byteArray.read_string();
			this.u16_2 = byteArray.read_uint16();
			this.u16_3 = byteArray.read_uint16();
			this.i32_5 = byteArray.read_int32();
			this.i32_6 = byteArray.read_int32();
			this.i32_7 = byteArray.read_int32();
			this.u32_1 = byteArray.read_uint32();
			this.u32_2 = byteArray.read_uint32();
			this.u32_3 = byteArray.read_uint32();
			this.i64_1 = byteArray.read_int64();
			this.i64_2 = byteArray.read_int64();
			this.gap_c = byteArray.read_string();
			this.i64_3 = byteArray.read_int64();
			this.u64_1 = byteArray.read_uint64();
			this.u64_2 = byteArray.read_uint64();
			this.u64_3 = byteArray.read_uint64();
			this.inte_1 = byteArray.read_integer();
			this.inte_2 = byteArray.read_integer();
			this.inte_3 = byteArray.read_integer();
			this.num_1 = byteArray.read_number();
			this.gap_d = byteArray.read_string();
			this.num_2 = byteArray.read_number();
			this.num_3 = byteArray.read_number();
			this.flt_1 = byteArray.read_float();
			this.flt_2 = byteArray.read_float();
			this.flt_3 = byteArray.read_float();
			this.dbl_1 = byteArray.read_double();
			this.dbl_2 = byteArray.read_double();
			this.dbl_3 = byteArray.read_double();
			this.str_1 = byteArray.read_string();
			this.str_2 = byteArray.read_string();
			this.str_3 = byteArray.read_string();
			this.gap_e = byteArray.read_int32();
			this.str_4 = byteArray.read_string();
			this.bin_1 = byteArray.read_binary();
			this.bin_2 = byteArray.read_binary();
			this.bin_3 = byteArray.read_binary();
			const hasuni_1 = byteArray.read_uint8() > 0;
			if (hasuni_1) {
				this.uni_1 = new union();
				this.uni_1.decode(byteArray);
			} else {
				this.uni_1 = null;
			}
			const hasuni_2 = byteArray.read_uint8() > 0;
			if (hasuni_2) {
				this.uni_2 = new union();
				this.uni_2.decode(byteArray);
			} else {
				this.uni_2 = null;
			}
			const hasuni_3 = byteArray.read_uint8() > 0;
			if (hasuni_3) {
				this.uni_3 = new union();
				this.uni_3.decode(byteArray);
			} else {
				this.uni_3 = null;
			}
			this.li32_1 = byteArray.read_list(() => byteArray.read_int32());
			this.li32_2 = byteArray.read_list(() => byteArray.read_int32());
			this.li32_3 = byteArray.read_list(() => byteArray.read_int32());
			this.lstr_1 = byteArray.read_list(() => byteArray.read_string());
			this.gap_f = byteArray.read_int32();
			this.lstr_2 = byteArray.read_list(() => byteArray.read_string());
			this.lstr_3 = byteArray.read_list(() => byteArray.read_string());
			this.lbin_1 = byteArray.read_list(() => byteArray.read_binary());
			this.lbin_2 = byteArray.read_list(() => byteArray.read_binary());
			this.lbin_3 = byteArray.read_list(() => byteArray.read_binary());
			this.lbool_1 = byteArray.read_list(() => byteArray.read_bool());
			this.lbool_2 = byteArray.read_list(() => byteArray.read_bool());
			this.lbool_3 = byteArray.read_list(() => byteArray.read_bool());
			this.lint_1 = byteArray.read_list(() => byteArray.read_integer());
			this.lint_2 = byteArray.read_list(() => byteArray.read_integer());
			this.lint_3 = byteArray.read_list(() => byteArray.read_integer());
			this.lnum_1 = byteArray.read_list(() => byteArray.read_number());
			this.lnum_2 = byteArray.read_list(() => byteArray.read_number());
			this.lnum_3 = byteArray.read_list(() => byteArray.read_number());
			this.lph_1 = byteArray.read_list(() => {
				const item = new phoneNumber();
				item.decode(byteArray);
				return item;
			});
			this.lph_2 = byteArray.read_list(() => {
				const item = new phoneNumber();
				item.decode(byteArray);
				return item;
			});
			this.gap_g = byteArray.read_string();
			this.lph_3 = byteArray.read_list(() => {
				const item = new phoneNumber();
				item.decode(byteArray);
				return item;
			});
			this.lleaf_1 = byteArray.read_list(() => {
				const item = new comboLeaf();
				item.decode(byteArray);
				return item;
			});
			this.lleaf_2 = byteArray.read_list(() => {
				const item = new comboLeaf();
				item.decode(byteArray);
				return item;
			});
			this.lleaf_3 = byteArray.read_list(() => {
				const item = new comboLeaf();
				item.decode(byteArray);
				return item;
			});
			this.ms_i_1 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.ms_i_2 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.ms_i_3 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.mi_s_1 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
			this.gap_h = byteArray.read_int32();
			this.mi_s_2 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
			this.mi_s_3 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
			this.ms_s_1 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
			this.ms_s_2 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
			this.ms_s_3 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
			this.mi_i_1 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.mi_i_2 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.mi_i_3 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.ms_b_1 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_binary();
				return __v;
			});
			this.ms_b_2 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_binary();
				return __v;
			});
			this.ms_b_3 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_binary();
				return __v;
			});
			this.mi_b_1 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_binary();
				return __v;
			});
			this.mi_b_2 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_binary();
				return __v;
			});
			this.mi_b_3 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_binary();
				return __v;
			});
			this.ms_p_1 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = new phoneNumber();
				__v.decode(byteArray);
				return __v;
			});
			this.ms_p_2 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = new phoneNumber();
				__v.decode(byteArray);
				return __v;
			});
			this.ms_p_3 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = new phoneNumber();
				__v.decode(byteArray);
				return __v;
			});
			this.mi_l_1 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = new comboLeaf();
				__v.decode(byteArray);
				return __v;
			});
			this.mi_l_2 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = new comboLeaf();
				__v.decode(byteArray);
				return __v;
			});
			this.mi_l_3 = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = new comboLeaf();
				__v.decode(byteArray);
				return __v;
			});
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.pick1 = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.pick1 = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.pick1 = [2, __ov];
						break;
					}
					case 3:
					{
						let __ov: any;
						__ov = byteArray.read_binary();
						this.pick1 = [3, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.gap_i = byteArray.read_int32();
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.pick2 = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.pick2 = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.pick2 = [2, __ov];
						break;
					}
					case 3:
					{
						let __ov: any;
						__ov = new phoneNumber();
						__ov.decode(byteArray);
						this.pick2 = [3, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.gap_j = byteArray.read_string();
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.pick3 = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.pick3 = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = new comboLeaf();
						__ov.decode(byteArray);
						this.pick3 = [2, __ov];
						break;
					}
					case 3:
					{
						let __ov: any;
						__ov = byteArray.read_list(() => byteArray.read_int32());
						this.pick3 = [3, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			const hasph_1 = byteArray.read_uint8() > 0;
			if (hasph_1) {
				this.ph_1 = new phoneNumber();
				this.ph_1.decode(byteArray);
			} else {
				this.ph_1 = null;
			}
			const hasph_2 = byteArray.read_uint8() > 0;
			if (hasph_2) {
				this.ph_2 = new phoneNumber();
				this.ph_2.decode(byteArray);
			} else {
				this.ph_2 = null;
			}
			this.gap_k = byteArray.read_string();
			const hasph_3 = byteArray.read_uint8() > 0;
			if (hasph_3) {
				this.ph_3 = new phoneNumber();
				this.ph_3.decode(byteArray);
			} else {
				this.ph_3 = null;
			}
			const hasleaf_1 = byteArray.read_uint8() > 0;
			if (hasleaf_1) {
				this.leaf_1 = new comboLeaf();
				this.leaf_1.decode(byteArray);
			} else {
				this.leaf_1 = null;
			}
			const hasleaf_2 = byteArray.read_uint8() > 0;
			if (hasleaf_2) {
				this.leaf_2 = new comboLeaf();
				this.leaf_2.decode(byteArray);
			} else {
				this.leaf_2 = null;
			}
			const hasleaf_3 = byteArray.read_uint8() > 0;
			if (hasleaf_3) {
				this.leaf_3 = new comboLeaf();
				this.leaf_3.decode(byteArray);
			} else {
				this.leaf_3 = null;
			}
			const hasnode_1 = byteArray.read_uint8() > 0;
			if (hasnode_1) {
				this.node_1 = new comboNode();
				this.node_1.decode(byteArray);
			} else {
				this.node_1 = null;
			}
			const hasnode_2 = byteArray.read_uint8() > 0;
			if (hasnode_2) {
				this.node_2 = new comboNode();
				this.node_2.decode(byteArray);
			} else {
				this.node_2 = null;
			}
			const hasnode_3 = byteArray.read_uint8() > 0;
			if (hasnode_3) {
				this.node_3 = new comboNode();
				this.node_3.decode(byteArray);
			} else {
				this.node_3 = null;
			}
			const haswrap_1 = byteArray.read_uint8() > 0;
			if (haswrap_1) {
				this.wrap_1 = new comboWrap();
				this.wrap_1.decode(byteArray);
			} else {
				this.wrap_1 = null;
			}
			const haswrap_2 = byteArray.read_uint8() > 0;
			if (haswrap_2) {
				this.wrap_2 = new comboWrap();
				this.wrap_2.decode(byteArray);
			} else {
				this.wrap_2 = null;
			}
			const haswrap_3 = byteArray.read_uint8() > 0;
			if (haswrap_3) {
				this.wrap_3 = new comboWrap();
				this.wrap_3.decode(byteArray);
			} else {
				this.wrap_3 = null;
			}
	}
}

export class benchLeaf {
	public static readonly PROTO_ID = 3001;
	public msgId: number = 3001;
		public id: number = 0;
		public tag: string = "";
		public blob: Uint8Array = new Uint8Array();

	public encode(byteArray: ByteArray): void {
			byteArray.write_int32(this.id);
			byteArray.write_string(this.tag);
			byteArray.write_binary(this.blob);
	}

	public decode(byteArray: ByteArray): void {
			this.id = byteArray.read_int32();
			this.tag = byteArray.read_string();
			this.blob = byteArray.read_binary();
	}
}

export class tBenchScalar {
	public static readonly PROTO_ID = 3002;
	public msgId: number = 3002;
		public a1: number = 0;
		public a2: number = 0;
		public a3: number = 0;
		public a4: number = 0;
		public a5: number = 0;
		public a6: number = 0;
		public a7: number = 0;
		public a8: number = 0;
		public b1: bigint = 0n;
		public b2: bigint = 0n;
		public b3: bigint = 0n;
		public b4: bigint = 0n;
		public s1: string = "";
		public s2: string = "";
		public s3: string = "";
		public s4: string = "";
		public p1: Uint8Array = new Uint8Array();
		public p2: Uint8Array = new Uint8Array();
		public p3: Uint8Array = new Uint8Array();
		public p4: Uint8Array = new Uint8Array();
		public f1: boolean = false;
		public f2: boolean = false;
		public f3: number = 0;
		public f4: number = 0;

	public encode(byteArray: ByteArray): void {
			byteArray.write_int32(this.a1);
			byteArray.write_int32(this.a2);
			byteArray.write_int32(this.a3);
			byteArray.write_int32(this.a4);
			byteArray.write_int32(this.a5);
			byteArray.write_int32(this.a6);
			byteArray.write_int32(this.a7);
			byteArray.write_int32(this.a8);
			byteArray.write_int64(this.b1);
			byteArray.write_int64(this.b2);
			byteArray.write_int64(this.b3);
			byteArray.write_int64(this.b4);
			byteArray.write_string(this.s1);
			byteArray.write_string(this.s2);
			byteArray.write_string(this.s3);
			byteArray.write_string(this.s4);
			byteArray.write_binary(this.p1);
			byteArray.write_binary(this.p2);
			byteArray.write_binary(this.p3);
			byteArray.write_binary(this.p4);
			byteArray.write_bool(this.f1);
			byteArray.write_bool(this.f2);
			byteArray.write_float(this.f3);
			byteArray.write_double(this.f4);
	}

	public decode(byteArray: ByteArray): void {
			this.a1 = byteArray.read_int32();
			this.a2 = byteArray.read_int32();
			this.a3 = byteArray.read_int32();
			this.a4 = byteArray.read_int32();
			this.a5 = byteArray.read_int32();
			this.a6 = byteArray.read_int32();
			this.a7 = byteArray.read_int32();
			this.a8 = byteArray.read_int32();
			this.b1 = byteArray.read_int64();
			this.b2 = byteArray.read_int64();
			this.b3 = byteArray.read_int64();
			this.b4 = byteArray.read_int64();
			this.s1 = byteArray.read_string();
			this.s2 = byteArray.read_string();
			this.s3 = byteArray.read_string();
			this.s4 = byteArray.read_string();
			this.p1 = byteArray.read_binary();
			this.p2 = byteArray.read_binary();
			this.p3 = byteArray.read_binary();
			this.p4 = byteArray.read_binary();
			this.f1 = byteArray.read_bool();
			this.f2 = byteArray.read_bool();
			this.f3 = byteArray.read_float();
			this.f4 = byteArray.read_double();
	}
}

export class tBenchList {
	public static readonly PROTO_ID = 3003;
	public msgId: number = 3003;
		public scores: number[] = [];
		public labels: string[] = [];
		public chunks: Uint8Array[] = [];
		public items: benchLeaf[] = [];
		public extras: comboLeaf[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_list(this.scores, (item) => byteArray.write_int32(item));
			byteArray.write_list(this.labels, (item) => byteArray.write_string(item));
			byteArray.write_list(this.chunks, (item) => byteArray.write_binary(item));
			byteArray.write_list(this.items, (item) => item.encode(byteArray));
			byteArray.write_list(this.extras, (item) => item.encode(byteArray));
	}

	public decode(byteArray: ByteArray): void {
			this.scores = byteArray.read_list(() => byteArray.read_int32());
			this.labels = byteArray.read_list(() => byteArray.read_string());
			this.chunks = byteArray.read_list(() => byteArray.read_binary());
			this.items = byteArray.read_list(() => {
				const item = new benchLeaf();
				item.decode(byteArray);
				return item;
			});
			this.extras = byteArray.read_list(() => {
				const item = new comboLeaf();
				item.decode(byteArray);
				return item;
			});
	}
}

export class tBenchMap {
	public static readonly PROTO_ID = 3004;
	public msgId: number = 3004;
		public attrs: Map<string, number> | null = null;
		public rev: Map<number, string> | null = null;
		public lookup: Map<string, benchLeaf> | null = null;
		public phones: Map<string, phoneNumber> | null = null;
		public tags: Map<string, string> | null = null;

	public encode(byteArray: ByteArray): void {
			byteArray.write_map(this.attrs, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.rev, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_string(v);
			});
			byteArray.write_map(this.lookup, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				v.encode(byteArray);
			});
			byteArray.write_map(this.phones, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				v.encode(byteArray);
			});
			byteArray.write_map(this.tags, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_string(v);
			});
	}

	public decode(byteArray: ByteArray): void {
			this.attrs = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.rev = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
			this.lookup = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = new benchLeaf();
				__v.decode(byteArray);
				return __v;
			});
			this.phones = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = new phoneNumber();
				__v.decode(byteArray);
				return __v;
			});
			this.tags = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
	}
}

export class tBenchOneof {
	public static readonly PROTO_ID = 3005;
	public msgId: number = 3005;
		public seq: number = 0;
		public reward: [number, any] | null = null;
		public meta: [number, any] | null = null;
		public payload: [number, any] | null = null;
		public tip: string = "";

	public encode(byteArray: ByteArray): void {
			byteArray.write_int32(this.seq);
			if (!this.reward) {
				byteArray.write_uint8(0);
			} else {
				switch (this.reward[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.reward[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.reward[1]);
						break;
					case 3:
						byteArray.write_uint8(3);
						this.reward[1].encode(byteArray);
						break;
					case 4:
						byteArray.write_uint8(4);
						this.reward[1].encode(byteArray);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			if (!this.meta) {
				byteArray.write_uint8(0);
			} else {
				switch (this.meta[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.meta[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.meta[1]);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			if (!this.payload) {
				byteArray.write_uint8(0);
			} else {
				switch (this.payload[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_binary(this.payload[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.payload[1]);
						break;
					case 3:
						byteArray.write_uint8(3);
						this.payload[1].encode(byteArray);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_string(this.tip);
	}

	public decode(byteArray: ByteArray): void {
			this.seq = byteArray.read_int32();
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.reward = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.reward = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.reward = [2, __ov];
						break;
					}
					case 3:
					{
						let __ov: any;
						__ov = new benchLeaf();
						__ov.decode(byteArray);
						this.reward = [3, __ov];
						break;
					}
					case 4:
					{
						let __ov: any;
						__ov = new phoneNumber();
						__ov.decode(byteArray);
						this.reward = [4, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.meta = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.meta = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.meta = [2, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.payload = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_binary();
						this.payload = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.payload = [2, __ov];
						break;
					}
					case 3:
					{
						let __ov: any;
						__ov = new benchLeaf();
						__ov.decode(byteArray);
						this.payload = [3, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.tip = byteArray.read_string();
	}
}

export class tBenchNested {
	public static readonly PROTO_ID = 3006;
	public msgId: number = 3006;
		public root: benchLeaf | null = null;
		public contact: phoneNumber | null = null;
		public leaf: comboLeaf | null = null;
		public children: benchLeaf[] = [];
		public contacts: phoneNumber[] = [];

	public encode(byteArray: ByteArray): void {
			if (this.root) {
				byteArray.write_uint8(1);
				this.root.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.contact) {
				byteArray.write_uint8(1);
				this.contact.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			if (this.leaf) {
				byteArray.write_uint8(1);
				this.leaf.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			byteArray.write_list(this.children, (item) => item.encode(byteArray));
			byteArray.write_list(this.contacts, (item) => item.encode(byteArray));
	}

	public decode(byteArray: ByteArray): void {
			const hasroot = byteArray.read_uint8() > 0;
			if (hasroot) {
				this.root = new benchLeaf();
				this.root.decode(byteArray);
			} else {
				this.root = null;
			}
			const hascontact = byteArray.read_uint8() > 0;
			if (hascontact) {
				this.contact = new phoneNumber();
				this.contact.decode(byteArray);
			} else {
				this.contact = null;
			}
			const hasleaf = byteArray.read_uint8() > 0;
			if (hasleaf) {
				this.leaf = new comboLeaf();
				this.leaf.decode(byteArray);
			} else {
				this.leaf = null;
			}
			this.children = byteArray.read_list(() => {
				const item = new benchLeaf();
				item.decode(byteArray);
				return item;
			});
			this.contacts = byteArray.read_list(() => {
				const item = new phoneNumber();
				item.decode(byteArray);
				return item;
			});
	}
}

export class tBenchHeavy {
	public static readonly PROTO_ID = 3007;
	public msgId: number = 3007;
		public seq: number = 0;
		public title: string = "";
		public root: benchLeaf | null = null;
		public items: benchLeaf[] = [];
		public scores: number[] = [];
		public labels: string[] = [];
		public chunks: Uint8Array[] = [];
		public attrs: Map<string, number> | null = null;
		public rev: Map<number, string> | null = null;
		public lookup: Map<string, benchLeaf> | null = null;
		public phones: Map<string, phoneNumber> | null = null;
		public reward: [number, any] | null = null;
		public meta: [number, any] | null = null;
		public payload: Uint8Array = new Uint8Array();
		public extras: comboLeaf[] = [];

	public encode(byteArray: ByteArray): void {
			byteArray.write_int32(this.seq);
			byteArray.write_string(this.title);
			if (this.root) {
				byteArray.write_uint8(1);
				this.root.encode(byteArray);
			} else {
				byteArray.write_uint8(0);
			}
			byteArray.write_list(this.items, (item) => item.encode(byteArray));
			byteArray.write_list(this.scores, (item) => byteArray.write_int32(item));
			byteArray.write_list(this.labels, (item) => byteArray.write_string(item));
			byteArray.write_list(this.chunks, (item) => byteArray.write_binary(item));
			byteArray.write_map(this.attrs, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				byteArray.write_int32(v);
			});
			byteArray.write_map(this.rev, (k) => {
				byteArray.write_int32(k);
			}, (v) => {
				byteArray.write_string(v);
			});
			byteArray.write_map(this.lookup, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				v.encode(byteArray);
			});
			byteArray.write_map(this.phones, (k) => {
				byteArray.write_string(k);
			}, (v) => {
				v.encode(byteArray);
			});
			if (!this.reward) {
				byteArray.write_uint8(0);
			} else {
				switch (this.reward[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.reward[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.reward[1]);
						break;
					case 3:
						byteArray.write_uint8(3);
						this.reward[1].encode(byteArray);
						break;
					case 4:
						byteArray.write_uint8(4);
						this.reward[1].encode(byteArray);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			if (!this.meta) {
				byteArray.write_uint8(0);
			} else {
				switch (this.meta[0]) {
					case 1:
						byteArray.write_uint8(1);
						byteArray.write_int32(this.meta[1]);
						break;
					case 2:
						byteArray.write_uint8(2);
						byteArray.write_string(this.meta[1]);
						break;
					default: throw new Error('invalid oneof');
				}
			}
			byteArray.write_binary(this.payload);
			byteArray.write_list(this.extras, (item) => item.encode(byteArray));
	}

	public decode(byteArray: ByteArray): void {
			this.seq = byteArray.read_int32();
			this.title = byteArray.read_string();
			const hasroot = byteArray.read_uint8() > 0;
			if (hasroot) {
				this.root = new benchLeaf();
				this.root.decode(byteArray);
			} else {
				this.root = null;
			}
			this.items = byteArray.read_list(() => {
				const item = new benchLeaf();
				item.decode(byteArray);
				return item;
			});
			this.scores = byteArray.read_list(() => byteArray.read_int32());
			this.labels = byteArray.read_list(() => byteArray.read_string());
			this.chunks = byteArray.read_list(() => byteArray.read_binary());
			this.attrs = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_int32();
				return __v;
			});
			this.rev = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_int32();
				return __k;
			}, () => {
				let __v: any;
				__v = byteArray.read_string();
				return __v;
			});
			this.lookup = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = new benchLeaf();
				__v.decode(byteArray);
				return __v;
			});
			this.phones = byteArray.read_map(() => {
				let __k: any;
				__k = byteArray.read_string();
				return __k;
			}, () => {
				let __v: any;
				__v = new phoneNumber();
				__v.decode(byteArray);
				return __v;
			});
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.reward = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.reward = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.reward = [2, __ov];
						break;
					}
					case 3:
					{
						let __ov: any;
						__ov = new benchLeaf();
						__ov.decode(byteArray);
						this.reward = [3, __ov];
						break;
					}
					case 4:
					{
						let __ov: any;
						__ov = new phoneNumber();
						__ov.decode(byteArray);
						this.reward = [4, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			{
				const __tag = byteArray.read_uint8();
				switch (__tag) {
					case 0:
						this.meta = null;
						break;
					case 1:
					{
						let __ov: any;
						__ov = byteArray.read_int32();
						this.meta = [1, __ov];
						break;
					}
					case 2:
					{
						let __ov: any;
						__ov = byteArray.read_string();
						this.meta = [2, __ov];
						break;
					}
					default: throw new Error(`invalid oneof tag: ${__tag}`);
				}
			}
			this.payload = byteArray.read_binary();
			this.extras = byteArray.read_list(() => {
				const item = new comboLeaf();
				item.decode(byteArray);
				return item;
			});
	}
}


export const ProtoMsgName = {
	1: 'test',
	2: 'phoneNumber',
	3: 'person',
	4: 'addressBook',
	5: 'union',
	6: 'tbool',
	7: 'tint8',
	8: 'tuint8',
	9: 'tint16',
	10: 'tuint16',
	11: 'tint32',
	12: 'tuint32',
	13: 'tint64',
	14: 'tuint64',
	15: 'tinteger',
	16: 'tnumber',
	17: 'tfloat',
	18: 'tdouble',
	19: 'tstring',
	20: 'tlistbool',
	21: 'tlistint8',
	22: 'tlistuint8',
	23: 'tlistint16',
	24: 'tlistuint16',
	25: 'tlistint32',
	26: 'tlistuint32',
	27: 'tlistint64',
	28: 'tlistuint64',
	29: 'tlistinteger',
	30: 'tlistnumber',
	31: 'tlistfloat',
	32: 'tlistdouble',
	33: 'tliststring',
	34: 'tlistunion',
	35: 'allType',
	36: 'testnull',
	37: 'tbinary',
	38: 'tmap',
	39: 'toneof',
	1001: 'person1',
	2001: 'comboLeaf',
	2002: 'comboNode',
	2003: 'comboWrap',
	2004: 'tMapMatrix',
	2005: 'tListMatrix',
	2006: 'tOneofMatrix',
	2007: 'tNestDeep',
	2008: 'tInterleave',
	2009: 'tMultiOneof',
	2010: 'tEmptyEdge',
	2011: 'tScalarRun',
	2012: 'tRepeatAll',
	3001: 'benchLeaf',
	3002: 'tBenchScalar',
	3003: 'tBenchList',
	3004: 'tBenchMap',
	3005: 'tBenchOneof',
	3006: 'tBenchNested',
	3007: 'tBenchHeavy',
};

export class MessageFactory {
	private static readonly messageConstructors: Record<number, new () => ProtoMessage> = {
		1: test,
		2: phoneNumber,
		3: person,
		4: addressBook,
		5: union,
		6: tbool,
		7: tint8,
		8: tuint8,
		9: tint16,
		10: tuint16,
		11: tint32,
		12: tuint32,
		13: tint64,
		14: tuint64,
		15: tinteger,
		16: tnumber,
		17: tfloat,
		18: tdouble,
		19: tstring,
		20: tlistbool,
		21: tlistint8,
		22: tlistuint8,
		23: tlistint16,
		24: tlistuint16,
		25: tlistint32,
		26: tlistuint32,
		27: tlistint64,
		28: tlistuint64,
		29: tlistinteger,
		30: tlistnumber,
		31: tlistfloat,
		32: tlistdouble,
		33: tliststring,
		34: tlistunion,
		35: allType,
		36: testnull,
		37: tbinary,
		38: tmap,
		39: toneof,
		1001: person1,
		2001: comboLeaf,
		2002: comboNode,
		2003: comboWrap,
		2004: tMapMatrix,
		2005: tListMatrix,
		2006: tOneofMatrix,
		2007: tNestDeep,
		2008: tInterleave,
		2009: tMultiOneof,
		2010: tEmptyEdge,
		2011: tScalarRun,
		2012: tRepeatAll,
		3001: benchLeaf,
		3002: tBenchScalar,
		3003: tBenchList,
		3004: tBenchMap,
		3005: tBenchOneof,
		3006: tBenchNested,
		3007: tBenchHeavy,
	};

	public static deserializeFromBytes(data: Uint8Array | number[]): ProtoMessage {
		const byteArray = new ByteArray();
		byteArray.setBytes(data, false);
		const msgId = byteArray.read_uint16();
		const Constructor = MessageFactory.messageConstructors[msgId];
		if (!Constructor) {
			throw new Error(`Unknown message ID: ${msgId}`);
		}
		const instance = new Constructor();
		instance.decode(byteArray);
		return instance;
	}

	public static serializeToBytes(message: ProtoMessage): Uint8Array {
		const byteArray = new ByteArray();
		byteArray.write_uint16(message.msgId);
		message.encode(byteArray);
		return byteArray.getBytes();
	}
}

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

-module(gTsGen).

-export([
   genTs/4
]).

spellClassHead(MsgName, MsgId) ->
   <<"export class ", MsgName/binary, " {\n",
     "\tpublic static readonly PROTO_ID = ", (integer_to_binary(MsgId))/binary, ";\n",
     "\tpublic msgId: number = ", (integer_to_binary(MsgId))/binary, ";\n">>.

spellClassMember(FieldList) ->
   <<<<(gTsField:builtMemberStr(OneTypeName))/binary>> || OneTypeName <- FieldList>>.

spellClassSerialize(FieldList) ->
   FunHead = <<"\n\tpublic encode(byteArray: ByteArray): void {\n">>,
   FunBody = <<<<(gTsField:builtEncodeStr(OneTypeName))/binary>> || OneTypeName <- FieldList>>,
   <<FunHead/binary, FunBody/binary, "\t}\n">>.

spellClassDeserialize(FieldList) ->
   FunHead = <<"\n\tpublic decode(byteArray: ByteArray): void {\n">>,
   FunBody = <<<<(gTsField:builtDecodeStr(OneTypeName))/binary>> || OneTypeName <- FieldList>>,
   <<FunHead/binary, FunBody/binary, "\t}\n">>.

spellClassEnd() ->
   <<"}\n\n">>.

genTs(SortedSProtoList, _SortedErrList, TsDir, _) ->
   FunSpell =
      fun({MsgName, MsgId, FieldList}, BinAcc) ->
         H = spellClassHead(MsgName, MsgId),
         M = spellClassMember(FieldList),
         E = spellClassSerialize(FieldList),
         D = spellClassDeserialize(FieldList),
         End = spellClassEnd(),
         <<BinAcc/binary, H/binary, M/binary, E/binary, D/binary, End/binary>>
      end,
   LastBinAcc = lists:foldl(FunSpell, <<>>, SortedSProtoList),
   
   %% 生成ByteArray工具类
   ByteArrayCode = spellByteArray(),
   
   %% 生成消息名称映射
    MsgNameCode = spellMsgNameMap(SortedSProtoList),
    
    %% 生成消息工厂
    MsgFactoryCode = spellMsgFactory(SortedSProtoList),
    
    FullCode = <<ByteArrayCode/binary, "\n\n", LastBinAcc/binary, "\n", MsgNameCode/binary, "\n", MsgFactoryCode/binary>>,
   
   TsFilename = do_write_ts(TsDir, protoMsg, FullCode),
   io:format("Generate TypeScript file: ~s ~n", [TsFilename]),
   ok.

spellByteArray() ->
<<"const textEncoder = new TextEncoder();
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
        
        // 预估最大长度 (utf8 一个字符最多4字节)
        const maxLength = value.length * 4;
        this.ensureCapacity(maxLength + 2);
        
        // 记录长度写入的位置
        const lengthPos = this.position;
        this.position += 2;
        
        // 使用 encodeInto 直接写入，避免内存分配
        const result = textEncoder.encodeInto(value, this.buffer.subarray(this.position));
        const written = result.written || 0;
        
        if (written > 65535) {
            throw new Error(`String too long: ${written} bytes (max: 65535)`);
        }
        
        // 回写长度
        this.view.setUint16(lengthPos, written, false);
        this.position += written;
    }

    // --- 复杂类型写入 (Integer / Number) ---

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
        // textDecoder 可以直接处理 subarray，这是零拷贝操作
        const str = textDecoder.decode(this.buffer.subarray(this.position, this.position + length));
        this.position += length;
        return str;
    }

    public read_integer(): number {
        const tag = this.read_uint8();
        switch (tag) {
            case 8: return this.read_int8();
            case 16: return this.read_int16();
            case 32: return this.read_int32();
            case 64: return Number(this.read_int64()); // 注意精度丢失风险
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

    // --- 列表写入辅助方法 (减少生成的代码量) ---

    public write_list<T>(list: T[] | null | undefined, writer: (val: T) => void): void {
        if (!list) {
            this.write_uint16(0);
            return;
        }
        this.write_uint16(list.length);
        for (let i = 0; i < list.length; i++) {
            writer.call(this, list[i]);
        }
    }

    public read_list<T>(reader: () => T): T[] {
        const length = this.read_uint16();
        const list = new Array<T>(length);
        for (let i = 0; i < length; i++) {
            list[i] = reader.call(this);
        }
        return list;
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
}">>.

spellMsgNameMap(SortedSProtoList) ->
   FunSpell =
      fun({MsgName, MsgId, _FieldList}, BinAcc) ->
         <<BinAcc/binary, "\t", (integer_to_binary(MsgId))/binary, ": '", MsgName/binary, "',\n">>
      end,
   Body = lists:foldl(FunSpell, <<>>, SortedSProtoList),
   <<"export const ProtoMsgName = {\n", Body/binary, "};\n">>.

spellMsgFactory(SortedSProtoList) ->
   FunSpell =
      fun({MsgName, MsgId, _FieldList}, BinAcc) ->
         <<BinAcc/binary, "\t\t", (integer_to_binary(MsgId))/binary, ": ", MsgName/binary, ",\n">>
      end,
   Body = lists:foldl(FunSpell, <<>>, SortedSProtoList),
   <<"export class MessageFactory {\n",
     "\tprivate static readonly messageConstructors = {\n",
     Body/binary,
     "\t};\n",
     "\n",
     "\tpublic static deserializeFromBytes(data: Uint8Array | number[]): any {\n",
     "\t\tconst byteArray = new ByteArray();\n",
     "\t\tbyteArray.setBytes(data, false);\n",
     "\t\tconst msgId = byteArray.read_uint16();\n",
     "\t\tconst Constructor = MessageFactory.messageConstructors[msgId];\n",
     "\t\tif (!Constructor) {\n",
     "\t\t\tthrow new Error(`Unknown message ID: ${msgId}`);\n",
     "\t\t}\n",
     "\t\tconst instance = new Constructor();\n",
     "\t\tinstance.decode(byteArray);\n",
     "\t\treturn instance;\n",
     "\t}\n",
     "\n",
     "\tpublic static serializeToBytes(message: any): Uint8Array {\n",
     "\t\tconst byteArray = new ByteArray();\n",
     "\t\tbyteArray.write_uint16(message.msgId);\n",
     "\t\tmessage.encode(byteArray);\n",
     "\t\treturn byteArray.getBytes();\n",
     "\t}\n",
     "}\n">>.

do_write_ts(TsDir, FileName, Content) ->
   FileNameBin = list_to_binary(atom_to_list(FileName)),
   FilePath = filename:join(TsDir, <<FileNameBin/binary, ".ts">>),
   ok = filelib:ensure_dir(FilePath),
   ok = file:write_file(FilePath, Content),
   FilePath.
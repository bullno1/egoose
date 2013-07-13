package com.massmify;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpInputStream;
import com.ericsson.otp.erlang.OtpOutputStream;
import com.gravity.goose.Article;
import com.gravity.goose.Configuration;
import com.gravity.goose.Goose;

public class GoosePort {
	public static void main(String[] args) throws IOException, OtpErlangDecodeException, InvalidArityException {
		GoosePort port = new GoosePort(
			new FileInputStream(FileDescriptor.in),
			new FileOutputStream(FileDescriptor.out)
		);
		port.run(args);
	}
	
	public GoosePort(InputStream inputStream, OutputStream outputStream) {
		in = inputStream;
		out = outputStream;
		otpOut = new OtpOutputStream();
		headerBuff = new byte[4];
		msgBuff = new byte[1024];
		otpIn = new OtpInputStream(msgBuff);
	}
	
	public void run(String[] args) throws IOException, OtpErlangDecodeException, InvalidArityException {
		while(true) {
			//read msg
			int msgLength = readMsgLength();
			if(msgLength <= 0) break;
			if(msgLength > msgBuff.length) {
				msgBuff = new byte[msgLength];
				otpIn = new OtpInputStream(msgBuff);
			}
			if(!readExact(msgBuff, msgLength)) break;
			otpIn.setPos(0);

			//process msg
			int tupleArity = otpIn.read_tuple_head();
			int functionArity = tupleArity - 1;
			String msgType = otpIn.read_atom();
			otpOut.reset();
			switch(msgType) {
			case "init": init(functionArity); break;
			case "echo": echo(functionArity); break;
			case "extract": extract(functionArity); break;
			}

			//send reply
			writeMsgLength(otpOut.getPos() + 1);
			out.write(MAGIC);
			otpOut.writeTo(out);
			out.flush();
		}
	}
	
	private void init(int arity) throws InvalidArityException, OtpErlangDecodeException {
		assertArity("init", 1, arity);

		Configuration conf = new Configuration();
		int numOpts = otpIn.read_list_head();
		for(int i = 0; i < numOpts; ++i) {
			int tupleArity = otpIn.read_tuple_head();
			if(tupleArity != 2)
				throw new IllegalArgumentException(
					"Expect list of 2-tuples, got a " + tupleArity + "-tuple at index " + i
				);
			String optName = otpIn.read_atom();
			switch(optName) {
			case "user_agent":
				conf.setBrowserUserAgent(readString());
				break;
			case "fetch_image":
				conf.setEnableImageFetching(otpIn.read_boolean());
				break; 
			case "connection_timeout":
				conf.setConnectionTimeout(otpIn.read_int());
				break; 
			case "imagemagick_convert_path":
				conf.setImagemagickConvertPath(readString());
				break;
			case "imagemagick_identify_path":
				conf.setImagemagickIdentifyPath(readString());
				break;
			case "local_storage_path":
				conf.setLocalStoragePath(readString());
				break;
			case "min_bytes_for_images":
				conf.setMinBytesForImages(otpIn.read_int());
				break;
			case "socket_timeout":
				conf.setSocketTimeout(otpIn.read_int());
				break;
			}
		}
		otpIn.read_nil();
		
		goose = new Goose(conf);
		otpOut.write_atom("ok");
	}
	
	private void extract(int arity) throws InvalidArityException, OtpErlangDecodeException{
		if(arity != 1 && arity != 2) {
			throw new InvalidArityException(
				"Wrong number of arguments for extract. Expect 1 or 2, got " + arity
			);
		}

		Article article = null;
		String url = new String(otpIn.read_binary(), UTF8);
		
		if(arity == 2) {
			String rawHTML = new String(otpIn.read_binary(), UTF8);
			article = goose.extractContent(url, rawHTML);
		}
		else {
			article = goose.extractContent(url);
		}

		otpOut.write_tuple_head(7);
		otpOut.write_atom("article");
		writeString(article.canonicalLink());//canonical_url
		writeString(article.title());//title
		writeString(article.metaDescription());//meta_description
		writeString(article.metaKeywords());//meta_keywords
		writeString(article.topImage().getImageSrc());//top_image
		writeString(article.cleanedArticleText());//body
	}
	
	private String readString() throws OtpErlangDecodeException {
		return new String(otpIn.read_binary(), UTF8);
	}
	
	private void writeString(String str) {
		otpOut.write_binary(str != null ? str.getBytes(UTF8) : EMPTY_STRING);
	}
	
	private void echo(int arity) throws OtpErlangDecodeException, InvalidArityException {
		assertArity("echo", 1, arity);
		OtpErlangObject arg = otpIn.read_any();
		otpOut.write_any(arg);
	}

	private int readMsgLength() throws IOException {
		if(!readExact(headerBuff, 4)) return -1;
		long length = 
			((headerBuff[0] & 0xFF) << 24) |
			((headerBuff[1] & 0xFF) << 16) |
			((headerBuff[2] & 0xFF) <<  8) |
			((headerBuff[3] & 0xFF) <<  0);
		return (int)length;
	}
	
	private boolean readExact(byte[] buff, int numBytes) throws IOException {
		int offset = 0;
		while(numBytes > 0) {
			int bytesRead = in.read(buff, offset, numBytes);
			if(bytesRead < 0) {
				return false;
			}
			else {
				offset += bytesRead;
				numBytes -= bytesRead;
			}
		}
		return true;
	}
	
	private void writeMsgLength(int length) throws IOException {
		out.write((byte)(length >>> 24));
		out.write((byte)(length >>> 16));
		out.write((byte)(length >>> 8));
		out.write((byte)length);
	}
	
	private static void assertArity(String function, int expected, int actual) throws InvalidArityException {
		if(expected != actual)
			throw new InvalidArityException(function, expected, actual);
	}
	
	private static final int MAGIC = 131;
	private static final Charset UTF8 = Charset.forName("UTF8");
	private static final byte[] EMPTY_STRING = "".getBytes(UTF8);
	private byte[] headerBuff;
	private byte[] msgBuff;
	private OtpOutputStream otpOut;
	private OtpInputStream otpIn;
	private InputStream in;
	private OutputStream out;
	private Goose goose;
}

package com.massmify;

public class InvalidArityException extends Exception {
	private static final long serialVersionUID = 1L;

	public InvalidArityException(String msg) {
		super(msg);
	}

	public InvalidArityException(String function, int expected, int actual) {
		super("Wrong number of arguments for " + function + ". Expected " + expected + ", got " + actual);
	}
}

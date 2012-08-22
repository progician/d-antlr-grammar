package dev.progician.dparser;

public class ParserTestAssertion extends Exception {
	/**
	 * The default serial UID 
	 */
	private static final long serialVersionUID = 7836825298412559689L;
	
	public String sourceFile = null;
	public String errorLine = null;
	public String expectedValue = null;
	public String actualValue = null;
	public int codeLine = -1;
	
	public ParserTestAssertion(String sourceFile, String errorLine, int codeLine, String expectedValue, String actualValue) {
		this.sourceFile = sourceFile;
		this.errorLine = errorLine;
		this.codeLine = codeLine;
		this.expectedValue = expectedValue;
		this.actualValue = actualValue;
	}

	public ParserTestAssertion(String sourceFile, String errorLine) {
		this.sourceFile = sourceFile;
		this.errorLine = errorLine;
	}
	
	public ParserTestAssertion(String sourceFile, String errorLine, int codeLine, String actualValue) {
		this.sourceFile = sourceFile;
		this.errorLine = errorLine;
		this.actualValue = actualValue;
	}

	public String toString() {
		
		String res;
		if (codeLine != -1)
			res = sourceFile + ':' + Integer.toString(codeLine) + ":"+ errorLine + "\n";
		else
			res = sourceFile + ':' + errorLine + "\n";
		
		if (expectedValue != null) {
			res += " -> expected: " + expectedValue + "\n";

			if (actualValue != null)
				res += " ->   actual: " + actualValue + "\n";
		}
		else {
			if (actualValue != null)
				res += " ->     line: " + actualValue + "\n";
		}
		

		return res;
	}
}

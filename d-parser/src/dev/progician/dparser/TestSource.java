package dev.progician.dparser;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.TokenRewriteStream;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.CommonToken;

import dev.progician.dparser.ParserTest.CheckReturn;

public class TestSource {
	private String sourcePath;
	private DeeParser parser;
	private CommonTree root;
	private String[] sourceLines;
	// Suppressed because it comes handy during debugging.
	@SuppressWarnings("unused")
	private String fullSource;
	
	public TestSource(String sourcePath) throws Exception {
		parseFile(sourcePath);
	}
	
	public void parseFile(String sourcePath) throws Exception {
		this.sourcePath = sourcePath;
		ANTLRFileStream fs = new ANTLRFileStream(this.sourcePath);
		DeeLexer lexer = new DeeLexer(fs);
		
		TokenRewriteStream tokens = new TokenRewriteStream(lexer);
		parser = new DeeParser(tokens);
		
		DeeParser.prog_return r = parser.prog();
		root =  (CommonTree) r.getTree();
		
		loadSourceLines();
		loadFullSource();
	}
	
	private void loadSourceLines() {
		try {
			FileInputStream fis = new FileInputStream(this.sourcePath);
			DataInputStream in = new DataInputStream(fis);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			ArrayList<String> lineArray = new ArrayList<String>();
			String line;
			while ((line = br.readLine()) != null) {
				lineArray.add(line);
			}
			
			sourceLines =  lineArray.toArray(new String[lineArray.size()]);
		} catch (IOException e) {
			// Should not happen!
			e.printStackTrace();
		}
	}
	
	private void loadFullSource() {
		try {
			FileInputStream fis = new FileInputStream(this.sourcePath);
			DataInputStream in = new DataInputStream(fis);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			char[] buff = new char[1024];
			int numRead;
			fullSource = "";
			while ((numRead = br.read(buff)) != -1) {
				String readData = String.valueOf(buff, 0, numRead);
				fullSource += readData;
			}
			
		} catch (IOException e) {
			// Should not happen!
			e.printStackTrace();
		}
	}

	public CheckReturn runTests() {
		System.out.print("checking " + sourcePath + " ... ");
		if (root.getChildren() == null) {
			System.err.println();
			System.err.println("error: file '" + sourcePath + "' has no testable AST structure!");
			return CheckReturn.ERROR;
		}
		
		CheckReturn ret = CheckReturn.OK;
		
		for (int lineIndex = 0; lineIndex < sourceLines.length; lineIndex++) {
			// Skip comment lines.
			if (sourceLines[lineIndex].startsWith("//"))
				continue;
			
			// Find the corresponding AST node for the current line.
			CommonTree node = getLineRoot(lineIndex, root);
			
			// Source element associated with this line.
			if (node != null) {
				try {
					// Test against expectation
					checkLine(lineIndex, node);
				}
				catch(ParserTestAssertion pta) {
					if (ret == CheckReturn.OK)
						System.out.println();
					
					System.err.print(pta.toString());
					ret = ret != CheckReturn.ERROR ? CheckReturn.WARNING : ret;
				}
			}
		}
		
		if (ret != CheckReturn.ERROR)
			System.out.println("[pass]");
		
		return ret;
	}
	
	public void checkLine(int codeLine, CommonTree astLine) throws ParserTestAssertion {
		ASTExpectation expect = null;
		String commentLine = null;
		for (int lIndex = codeLine - 1; lIndex >= 0; lIndex--) {
			if (sourceLines[lIndex].isEmpty())
				continue;
			
			// If there's a comment line above, parse it up as ASTExpectation.
			if (sourceLines[lIndex].startsWith("//")) {
				commentLine = sourceLines[lIndex].substring(2);
				expect = new ASTExpectation(commentLine);
			}
			
			break;
		}
		
		if (expect == null)
			throw new ParserTestAssertion(sourcePath, "code line is missing expected values", codeLine, sourceLines[codeLine]);
		
		if (expect.isValid()) {
			if (!expect.checkAST(astLine))
					throw new ParserTestAssertion(sourcePath, "test failed", codeLine, commentLine, astLine.toStringTree());
		}
	}
	
	private CommonTree getLineRoot(int codeLine, CommonTree node) {
		int lineStart = getLinePos(codeLine);
		int lineEnd = lineStart + sourceLines[codeLine].length() - 1;
		
		int nodeStart = getSourceRange(node).getStartIndex();
		int nodeEnd = getSourceRange(node).getEndIndex();
		
		if (nodeStart == lineStart && nodeEnd == lineEnd)
			return node;
		else if (lineStart >= nodeStart && lineEnd <= nodeEnd) {
			if (node.getChildren() != null) {
				for (Object o : node.getChildren()) {
					CommonTree child = (CommonTree) o;
					CommonTree r = getLineRoot(codeLine, child);
					if (r != null)
						return r;
				}
				
				return null;
			}
		}
		
		return null;
	}
	
	private int getLinePos(int codeLine) {
		int res = 0;
		for (int l = 0; l < codeLine; l++) {
			res += sourceLines[l].length() + 2;
		}
		
		return res;
	}
	
	private SourceRange getSourceRange(CommonTree node) {
		CommonToken startToken = (CommonToken) parser.getTokenStream().get(node.getTokenStartIndex());
		CommonToken stopToken = (CommonToken) parser.getTokenStream().get(node.getTokenStopIndex());
		return new SourceRange(startToken.getStartIndex(), stopToken.getStopIndex());
	}
}

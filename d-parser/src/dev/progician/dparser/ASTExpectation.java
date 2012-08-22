package dev.progician.dparser;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.TokenRewriteStream;
import org.antlr.runtime.tree.CommonTree;

import dev.progician.dparser.TokenTreeParser.tokenTree_return;

/**
 * <p>Class that is responsible to check a sub-tree of the AST by a
 * strictly formatted expectation string. The expectation tree can be
 * built without this formatted AST descriptor string by assigning the
 * rootNode directly to a non-null CommonTree object.</p>
 * @author progician
 *
 */
public class ASTExpectation {
	/** The string representing the expectation. */
	public String value;
	/** The state of the object. If it is null, the object is invalid. */
	public CommonTree rootNode;
	
	/** Checks if the expectation is valid. */
	public boolean isValid() {
		return rootNode != null;
	}
	
	/** Constructing the object from a single string. */
	public ASTExpectation(String value) {
		parse(value);
	}
	
	/** Constructing with invalid state. */
	public ASTExpectation() {
		this.value = null;
		this.rootNode = null;
	}
	
	/** Constructing directly with a root node of a custom built expectation tree. */
	public ASTExpectation(CommonTree rootNode) {
		this.value = null;
		this.rootNode = null;
	}
	
	/**
	 * Initialize the object with an expectation string. In case of parsing error
	 * report it directly to the console and the object will be invalid.
	 * @param value The expectation string.
	 */
	public void parse(String value) {
		// Initialize the state of the object at parsing
		this.value = value;
		this.rootNode = null;
		
		// Parsing up the line
		ANTLRStringStream stringStream = new ANTLRStringStream(this.value);
		TokenTreeLexer lexer = new TokenTreeLexer(stringStream);
		TokenRewriteStream tokens = new TokenRewriteStream(lexer);
		TokenTreeParser parser = new TokenTreeParser(tokens);
		
		// Initializing the state of the expectation object.
		try {
			tokenTree_return ret = parser.tokenTree();
			rootNode = (CommonTree) ret.getTree();
		}
		catch (RecognitionException ex) {
			// The rootNode should stay null in case exception thus the object is invalid.
			System.err.println(ex.toString());
		}
	}
	
	/**
	 * Compares the expectation tree with the actual AST subtree. 
	 * @param subTree The AST subtree.
	 * @return True if the AST and the expectation matches; otherwise false.
	 */
	public boolean checkAST(CommonTree subTree) {
		return checkASTRecursive(rootNode, subTree);
	}
	
	private static boolean checkASTRecursive(CommonTree expectation, CommonTree actualTree) {
		// Check the token identity first.
		String tokenIdentity = expectation.getToken().getText();
		String tokenType = DeeParser.tokenNames[actualTree.getToken().getType()];
		if (!tokenIdentity.equals(tokenType))
			return false;

		int startIndex = 0;
		if (expectation.getChildren() != null) {
			if (expectation.getChild(0).getType() == TokenTreeParser.StringLiteral) {
				// Get the text value of the token.
				String expValue = expectation.getChild(0).getText();
				expValue = expValue.substring(1, expValue.length() - 1);
				String tokenValue = actualTree.getToken().getText();
				
				// Breaks the expectation if the token's value specified and differs from the AST
				if (!expValue.equals(tokenValue))
					return false;
				
				// The real children will start on the index #1.
				startIndex = 1;
			}
		}
		
		// If the child count differs the check fails.
		if ((expectation.getChildCount() - startIndex) != actualTree.getChildCount())
			return false;
		
		// Recursive iteration through the child nodes.
		for (int idx = startIndex; idx < expectation.getChildCount(); idx++) {
			if (!checkASTRecursive((CommonTree) expectation.getChild(idx), (CommonTree) actualTree.getChild(idx - startIndex)))
				return false;
		}
		
		return true;
	}
}

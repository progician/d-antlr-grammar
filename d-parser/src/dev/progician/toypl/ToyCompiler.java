package dev.progician.toypl;

import java.io.File;
import java.io.IOException;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.TokenRewriteStream;
import org.antlr.runtime.tree.CommonTree;

public class ToyCompiler {
	
	public static final String testDir = "test-sources";  
	
	/**
	 *  Entry point for the application.
	 * @param args The command line arguments.
	 */
	public static void main(String[] args) {
		ToyCompiler tc = new ToyCompiler();
		tc.run();
	}
	
	private CommonTree parse(String fileName) {
		try {
			ANTLRFileStream fs = new ANTLRFileStream(testDir +"/" + fileName);
			toyplLexer lexer = new toyplLexer(fs);
			
			TokenRewriteStream tokens = new TokenRewriteStream(lexer);
			toyplParser parser = new toyplParser(tokens);
			
			toyplParser.prog_return r = parser.prog();
			CommonTree root =  (CommonTree) r.getTree();
			return root;
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		} catch (RecognitionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
	}
	
	private void run() {
		File dir = new File(testDir);
		String[] ls = dir.list();
		if (ls == null)
			System.err.println("error(1): test-sources directory doesn't exist or is not directory!");
		else {
			for (String entryName : ls) {
				System.out.println("Parsing " + entryName + " ...");
				CommonTree root = parse(entryName);
				if (root.getChildren() != null) {
					for (Object def : root.getChildren()) {
						CommonTree node = (CommonTree) def;
						System.out.println(node.toStringTree());
					}
				}
			}
		}
	}

}

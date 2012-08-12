package dev.progician.dparser;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.TokenRewriteStream;
import org.antlr.runtime.tree.CommonTree;

public class ParserTest {
	
	public static final String testSourceDir = "test-sources";  
	
	/**
	 *  Entry point for the application.
	 * @param args The command line arguments.
	 */
	public static void main(String[] args) {
		ParserTest tc = new ParserTest();
		tc.run();
	}
	
	private CommonTree parse(String fileName) {
		try {
			ANTLRFileStream fs = new ANTLRFileStream(testSourceDir +"/" + fileName);
			DeeLexer lexer = new DeeLexer(fs);
			
			TokenRewriteStream tokens = new TokenRewriteStream(lexer);
			DeeParser parser = new DeeParser(tokens);
			
			DeeParser.prog_return r = parser.prog();
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
		File dir = new File(testSourceDir);
		String[] ls = dir.list();
		if (ls == null)
			System.err.println("error(1): test-sources directory doesn't exist or is not directory!");
		else {
			boolean runResults = true;
			
			for (String entryName : ls) {
				System.out.print("checking " + entryName + " ... ");
				CommonTree root = parse(entryName);
				if (root.getChildren() == null) {
					System.err.println();
					System.err.println("error: file '" + entryName + "' has no testable AST structure!");
					continue;
				}
				else {
					String[] lines = loadTestLines(entryName);
					CheckReturn ret = checkCorrespondingLines(entryName, lines, root);
					if (ret == CheckReturn.OK)
						System.out.println("[pass]");
					else if (ret == CheckReturn.ERROR)
						runResults = false;
				}
			}
			
			if (runResults)
				System.out.println("Tests succeeded!");
			else
				System.out.println("Tests failed!");
		}
	}
	
	private String[] loadTestLines(String fileName) {
		try {
			FileInputStream fis = new FileInputStream(testSourceDir +"/" + fileName);
			DataInputStream in = new DataInputStream(fis);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));
			ArrayList<String> lineArray = new ArrayList<String>();
			String line;
			while ((line = br.readLine()) != null) {
				lineArray.add(line);
			}
			
			return lineArray.toArray(new String[lineArray.size()]);
		} catch (IOException e) {
			// Should not happen!
			e.printStackTrace();
		}
		
		return null;
	}
	
	enum CheckReturn {
		OK,
		WARNING,
		ERROR
	}
	
	private static CheckReturn checkCorrespondingLines(String fileName, String[] sourceLines, CommonTree rootNode) {
		CheckReturn ret = CheckReturn.OK;
		
		for (Object entry : rootNode.getChildren()) {
			CommonTree node = (CommonTree) entry;
			int codeLine = node.getLine() - 1;
			String commentLine = null;
			for (int lIndex = codeLine - 1; lIndex >= 0; lIndex--) {
				if (sourceLines[lIndex].isEmpty())
					continue;
				
				if (sourceLines[lIndex].startsWith("// "))
					commentLine = normalizeASTString(sourceLines[lIndex].substring(3));
				
				break;
			}
			
			if (commentLine == null) {
				ret = ret != CheckReturn.ERROR ? CheckReturn.WARNING : ret;
				System.out.println();
				System.out.println(fileName + ":" + Integer.toString(codeLine) + ": warning: code line missing AST description comment");
				System.out.println("    -> declDef: " + node.toStringTree());
			}
			else {
				String stringTree = node.toStringTree(); 
				if (commentLine.equals(stringTree) == false) {
					if (commentLine.length() == stringTree.length()) {
						for (int charIdx = 0; charIdx < commentLine.length(); charIdx++) {
							if (commentLine.charAt(charIdx) != stringTree.charAt(charIdx)) {
								System.err.println();
								System.err.println(fileName + ":" + Integer.toString(codeLine) + ": error: code line produces different AST than specified");
								System.err.println("    -> declDef: " + stringTree);
								System.err.println("    ->        AST: " + commentLine);
								ret = CheckReturn.ERROR;
								break;
							}
						}
					}
					else {
						System.err.println();
						System.err.println(fileName + ":" + Integer.toString(codeLine) + ": error: code line produces different AST than specified");
						System.err.println("    -> declDef: " + stringTree);
						System.err.println("    ->        AST: " + commentLine);
						ret = CheckReturn.ERROR;
					}
				}
			}
		}
		
		return ret;
	}
	
	private static String normalizeASTString(String desc) {
		String res = "";
		for (int idx = 0; idx < desc.length(); idx++) {
			if (desc.charAt(idx) == ' ' && (idx > 0 && desc.charAt(idx - 1) == ' '))
				continue;
			
			res += desc.charAt(idx);
		}
		
		res = res.trim();
		
		return res;
	}
}

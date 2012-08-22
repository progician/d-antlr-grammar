package dev.progician.dparser;

import java.io.File;

public class ParserTest {
	
	public static final String testSourceDir = "test-sources";  
	
	/**
	 *  Entry point for the application.
	 * @param args The command line arguments.
	 */
	public static void main(String[] args) throws Exception {
		ParserTest tc = new ParserTest();
		tc.run();
	}
	
	private void run() throws Exception{
		File dir = new File(testSourceDir);
		String[] ls = dir.list();
		if (ls == null)
			System.err.println("error(1): test-sources directory doesn't exist or is not directory!");
		else {
			CheckReturn res = CheckReturn.OK;
			
			for (String entryName : ls) {
				TestSource testSource = new TestSource(testSourceDir + "/" + entryName);
				CheckReturn ret = testSource.runTests();
				if (res == CheckReturn.OK)
					res = ret;
				else if (res == CheckReturn.WARNING && ret == CheckReturn.ERROR)
					res = ret;
			}
			
			if (res != CheckReturn.ERROR)
				System.out.println("Tests succeeded!");
			else
				System.out.println("Tests failed!");
		}
	}
	
	enum CheckReturn {
		OK,
		WARNING,
		ERROR
	}
}

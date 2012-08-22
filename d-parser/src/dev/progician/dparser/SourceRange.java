package dev.progician.dparser;

public class SourceRange {
	private int startIndex;
	public int getStartIndex() {
		return startIndex;
	}
	public void setStartIndex(int v) {
		startIndex = v;
	}
	
	private int endIndex;
	public int getEndIndex() {
		return endIndex;
	}
	public void setEndIndex(int v) {
		endIndex = v;
	}
	
	public SourceRange() {
		startIndex = endIndex = -1;
	}
	
	public SourceRange(int start, int end) {
		this.startIndex = start;
		this.endIndex = end;
	}
	
	public int getLength() {
		if (this.startIndex == -1 || this.endIndex == -1)
			return 0;
		
		return this.getStartIndex() - this.getEndIndex();
	}
}

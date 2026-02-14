You are a code review expert analyzing a git diff to assess confidence in the quality of the changes.

Analyze the following diff and return a JSON object with your confidence assessment. Consider:

1. **Code completeness**: Are there TODO comments, placeholder implementations, or stub functions?
2. **Error handling**: Are errors properly checked and handled?
3. **Consistency**: Do the changes follow consistent patterns with the existing codebase?
4. **Complexity**: Are changes appropriately scoped, or do they mix unrelated concerns?
5. **Testing signals**: Do changes to logic files have corresponding test changes?

Return ONLY a JSON object with this exact structure (no markdown, no explanation):

{
  "overall": 0.85,
  "per_file": {
    "path/to/file.go": 0.9,
    "path/to/other.ts": 0.7
  },
  "reasoning": "Brief explanation of the overall score and key factors."
}

Rules:
- "overall" must be a float between 0.0 and 1.0
- "per_file" must include every changed file from the diff
- Each per-file score must be between 0.0 and 1.0
- "reasoning" should be 1-3 sentences
- Return ONLY valid JSON, no markdown fences or additional text

import groovy.json.JsonSlurper
import java.nio.file.Files

class DataProcessor {
    def jsonParser = new JsonSlurper()
    
    def parseData(String input) {
        return jsonParser.parseText(input)
    }
    
    String formatOutput(Map data) {
        return data.collect { k, v -> "${k}: ${v}" }.join("\n")
    }
    
    private void logInternal(String msg) {
        println "[LOG] ${msg}"
    }
}

def processFile(String path) {
    def content = new File(path).text
    def processor = new DataProcessor()
    return processor.parseData(content)
}

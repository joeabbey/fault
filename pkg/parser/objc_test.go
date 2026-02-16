package parser

import (
	"os"
	"testing"
)

func TestObjCParserLanguage(t *testing.T) {
	p := NewObjCParser()
	if p.Language() != "objc" {
		t.Errorf("expected objc, got %q", p.Language())
	}
}

func TestObjCParserImports(t *testing.T) {
	src := `#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>
#import "AppDelegate.h"
@import CoreData;
`

	p := NewObjCParser()
	pf, err := p.Parse("main.m", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	if len(pf.Imports) != 4 {
		t.Fatalf("expected 4 imports, got %d", len(pf.Imports))
	}

	expected := []string{"Foundation/Foundation.h", "UIKit/UIKit.h", "AppDelegate.h", "CoreData"}
	for i, exp := range expected {
		if pf.Imports[i].Path != exp {
			t.Errorf("import %d: expected path %q, got %q", i, exp, pf.Imports[i].Path)
		}
	}
}

func TestObjCParserInterface(t *testing.T) {
	src := `@interface NetworkManager : NSObject <NSURLSessionDelegate>

@property (nonatomic, strong) NSString *baseURL;
@property (nonatomic, assign) NSInteger timeout;

+ (instancetype)sharedManager;
- (void)fetchData;
- (BOOL)postData:(NSData *)data toEndpoint:(NSString *)endpoint;

@end
`

	p := NewObjCParser()
	pf, err := p.Parse("NetworkManager.h", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	// Check interface
	symMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symMap[sym.Name] = sym
	}

	if sym, ok := symMap["NetworkManager"]; !ok {
		t.Error("expected NetworkManager class not found")
	} else if sym.Kind != "class" {
		t.Errorf("NetworkManager: expected kind 'class', got %q", sym.Kind)
	}

	// Check exports
	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}
	if !exportNames["NetworkManager"] {
		t.Error("NetworkManager should be exported")
	}

	// Check properties
	propNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		if sym.Kind == "variable" {
			propNames[sym.Name] = true
		}
	}
	if !propNames["baseURL"] {
		t.Error("expected baseURL property not found")
	}
	if !propNames["timeout"] {
		t.Error("expected timeout property not found")
	}
}

func TestObjCParserProtocol(t *testing.T) {
	src := `@protocol DataSourceDelegate <NSObject>

- (NSInteger)numberOfItems;
- (NSString *)itemAtIndex:(NSInteger)index;

@optional
- (void)didSelectItem:(NSString *)item;

@end

@protocol Configurable

- (void)configure:(NSDictionary *)options;

@end
`

	p := NewObjCParser()
	pf, err := p.Parse("Protocols.h", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	symMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		if sym.Kind == "interface" {
			symMap[sym.Name] = sym
		}
	}

	if _, ok := symMap["DataSourceDelegate"]; !ok {
		t.Error("expected DataSourceDelegate protocol not found")
	}
	if _, ok := symMap["Configurable"]; !ok {
		t.Error("expected Configurable protocol not found")
	}

	// Check methods
	methodNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		if sym.Kind == "method" {
			methodNames[sym.Name] = true
		}
	}
	if !methodNames["numberOfItems"] {
		t.Error("expected numberOfItems method not found")
	}
	if !methodNames["itemAtIndex:"] {
		t.Error("expected itemAtIndex: method not found")
	}
	if !methodNames["didSelectItem:"] {
		t.Error("expected didSelectItem: method not found")
	}
	if !methodNames["configure:"] {
		t.Error("expected configure: method not found")
	}
}

func TestObjCParserMethods(t *testing.T) {
	src := `@implementation MyClass

+ (instancetype)sharedInstance {
    return nil;
}

- (void)doSomething {
}

- (BOOL)isValid {
    return YES;
}

@end
`

	p := NewObjCParser()
	pf, err := p.Parse("MyClass.m", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	methodNames := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		if sym.Kind == "method" {
			methodNames[sym.Name] = sym
		}
	}

	if sym, ok := methodNames["sharedInstance"]; !ok {
		t.Error("expected sharedInstance method not found")
	} else if sym.Signature != "+sharedInstance" {
		t.Errorf("sharedInstance: expected signature '+sharedInstance', got %q", sym.Signature)
	}

	if sym, ok := methodNames["doSomething"]; !ok {
		t.Error("expected doSomething method not found")
	} else if sym.Signature != "-doSomething" {
		t.Errorf("doSomething: expected signature '-doSomething', got %q", sym.Signature)
	}

	if _, ok := methodNames["isValid"]; !ok {
		t.Error("expected isValid method not found")
	}
}

func TestObjCParserBlockComments(t *testing.T) {
	src := `/* Block comment
 * @interface FakeClass : NSObject
 * should not be parsed
 */
@interface RealClass : NSObject
@end
`

	p := NewObjCParser()
	pf, err := p.Parse("main.m", []byte(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	for _, sym := range pf.Symbols {
		if sym.Name == "FakeClass" {
			t.Error("FakeClass should not be parsed (inside block comment)")
		}
	}

	found := false
	for _, sym := range pf.Symbols {
		if sym.Name == "RealClass" {
			found = true
		}
	}
	if !found {
		t.Error("expected RealClass not found")
	}
}

func TestObjCParserSampleFile(t *testing.T) {
	content, err := os.ReadFile("../../testdata/objc/sample.m")
	if err != nil {
		t.Fatalf("failed to read testdata: %v", err)
	}

	p := NewObjCParser()
	pf, err := p.Parse("testdata/objc/sample.m", content)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}

	// Verify imports: Foundation/Foundation.h, UIKit/UIKit.h, AppDelegate.h, ViewController.h,
	//                 CoreData, MapKit
	if len(pf.Imports) != 6 {
		t.Errorf("expected 6 imports, got %d", len(pf.Imports))
		for _, imp := range pf.Imports {
			t.Logf("  import: %s", imp.Path)
		}
	}

	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}
	for _, expected := range []string{"Foundation/Foundation.h", "UIKit/UIKit.h", "AppDelegate.h", "CoreData", "MapKit"} {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found", expected)
		}
	}

	// Check that interfaces and protocols are found
	symMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symMap[sym.Name] = sym
	}

	if sym, ok := symMap["NetworkManager"]; !ok {
		t.Error("expected NetworkManager not found")
	} else if sym.Kind != "class" {
		t.Errorf("NetworkManager: expected kind 'class', got %q", sym.Kind)
	}

	if sym, ok := symMap["CacheManager"]; !ok {
		t.Error("expected CacheManager not found")
	} else if sym.Kind != "class" {
		t.Errorf("CacheManager: expected kind 'class', got %q", sym.Kind)
	}

	if sym, ok := symMap["DataSourceDelegate"]; !ok {
		t.Error("expected DataSourceDelegate not found")
	} else if sym.Kind != "interface" {
		t.Errorf("DataSourceDelegate: expected kind 'interface', got %q", sym.Kind)
	}

	if sym, ok := symMap["Configurable"]; !ok {
		t.Error("expected Configurable not found")
	} else if sym.Kind != "interface" {
		t.Errorf("Configurable: expected kind 'interface', got %q", sym.Kind)
	}

	// Check exports
	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}
	if !exportNames["NetworkManager"] {
		t.Error("NetworkManager should be exported")
	}
	if !exportNames["DataSourceDelegate"] {
		t.Error("DataSourceDelegate should be exported")
	}

	// Verify methods exist
	methodNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		if sym.Kind == "method" {
			methodNames[sym.Name] = true
		}
	}
	if !methodNames["sharedManager"] {
		t.Error("expected sharedManager method not found")
	}
	if !methodNames["fetchDataWithCompletion:"] {
		t.Error("expected fetchDataWithCompletion: method not found")
	}
}

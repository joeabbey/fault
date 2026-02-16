package parser

import (
	"testing"
)

func TestKotlinParserLanguage(t *testing.T) {
	p := NewKotlinParser()
	if p.Language() != "kotlin" {
		t.Errorf("expected kotlin, got %q", p.Language())
	}
}

func TestKotlinParserImports(t *testing.T) {
	code := `package com.example.app

import java.util.List
import kotlinx.coroutines.*
import java.util.Date as JDate
import kotlin.collections.MutableMap
`
	p := NewKotlinParser()
	pf, err := p.Parse("App.kt", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	if pf.Language != "kotlin" {
		t.Errorf("expected language kotlin, got %q", pf.Language)
	}

	importPaths := make(map[string]bool)
	for _, imp := range pf.Imports {
		importPaths[imp.Path] = true
	}

	expectedImports := []string{
		"java.util.List",
		"kotlinx.coroutines.*",
		"java.util.Date",
		"kotlin.collections.MutableMap",
	}
	for _, expected := range expectedImports {
		if !importPaths[expected] {
			t.Errorf("expected import %q not found; have: %v", expected, importPaths)
		}
	}

	if len(pf.Imports) != 4 {
		t.Errorf("expected 4 imports, got %d", len(pf.Imports))
	}

	// Check aliased import
	for _, imp := range pf.Imports {
		if imp.Path == "java.util.Date" {
			found := false
			for _, n := range imp.Names {
				if n == "JDate" {
					found = true
				}
			}
			if !found {
				t.Error("expected aliased import java.util.Date to have name 'JDate'")
			}
		}
	}

	// Check wildcard import names
	for _, imp := range pf.Imports {
		if imp.Path == "kotlinx.coroutines.*" {
			if len(imp.Names) != 0 {
				t.Errorf("expected wildcard import to have no names, got %v", imp.Names)
			}
		}
	}
}

func TestKotlinParserClasses(t *testing.T) {
	code := `package com.example

class Simple
data class User(val name: String, val age: Int)
sealed class Result
abstract class Base
open class Parent
annotation class MyAnnotation
private class Internal
enum class Color { RED, GREEN, BLUE }
`
	p := NewKotlinParser()
	pf, err := p.Parse("Types.kt", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	// Check Simple class
	if sym, ok := symbolMap["Simple"]; !ok {
		t.Error("expected symbol 'Simple' not found")
	} else {
		if sym.Kind != "class" {
			t.Errorf("expected Simple kind=class, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected Simple to be exported (default visibility)")
		}
	}

	// Check data class
	if sym, ok := symbolMap["User"]; !ok {
		t.Error("expected symbol 'User' not found")
	} else {
		if sym.Kind != "data_class" {
			t.Errorf("expected User kind=data_class, got %q", sym.Kind)
		}
	}

	// Check sealed class
	if _, ok := symbolMap["Result"]; !ok {
		t.Error("expected symbol 'Result' not found")
	}

	// Check annotation class
	if sym, ok := symbolMap["MyAnnotation"]; !ok {
		t.Error("expected symbol 'MyAnnotation' not found")
	} else {
		if sym.Kind != "annotation" {
			t.Errorf("expected MyAnnotation kind=annotation, got %q", sym.Kind)
		}
	}

	// Check private class is not exported
	if sym, ok := symbolMap["Internal"]; !ok {
		t.Error("expected symbol 'Internal' not found")
	} else {
		if sym.Exported {
			t.Error("expected Internal to NOT be exported (private)")
		}
	}

	// Check enum class
	if sym, ok := symbolMap["Color"]; !ok {
		t.Error("expected symbol 'Color' not found")
	} else {
		if sym.Kind != "enum" {
			t.Errorf("expected Color kind=enum, got %q", sym.Kind)
		}
	}
}

func TestKotlinParserInterfaces(t *testing.T) {
	code := `package com.example

interface Clickable {
    fun onClick()
}

fun interface Predicate {
    fun test(value: Int): Boolean
}

sealed interface Event
`
	p := NewKotlinParser()
	pf, err := p.Parse("Interfaces.kt", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	if sym, ok := symbolMap["Clickable"]; !ok {
		t.Error("expected symbol 'Clickable' not found")
	} else {
		if sym.Kind != "interface" {
			t.Errorf("expected Clickable kind=interface, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected Clickable to be exported")
		}
	}

	if sym, ok := symbolMap["Predicate"]; !ok {
		t.Error("expected symbol 'Predicate' not found")
	} else {
		if sym.Kind != "interface" {
			t.Errorf("expected Predicate kind=interface, got %q", sym.Kind)
		}
	}

	if _, ok := symbolMap["Event"]; !ok {
		t.Error("expected symbol 'Event' not found")
	}
}

func TestKotlinParserObjects(t *testing.T) {
	code := `package com.example

object Singleton {
    val instance = "test"
}

class MyClass {
    companion object Factory {
        fun create(): MyClass = MyClass()
    }
}
`
	p := NewKotlinParser()
	pf, err := p.Parse("Objects.kt", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	if sym, ok := symbolMap["Singleton"]; !ok {
		t.Error("expected symbol 'Singleton' not found")
	} else {
		if sym.Kind != "object" {
			t.Errorf("expected Singleton kind=object, got %q", sym.Kind)
		}
	}

	if sym, ok := symbolMap["Factory"]; !ok {
		t.Error("expected symbol 'Factory' not found")
	} else {
		if sym.Kind != "object" {
			t.Errorf("expected Factory kind=object, got %q", sym.Kind)
		}
	}
}

func TestKotlinParserFunctions(t *testing.T) {
	code := `package com.example

fun greet(name: String): String {
    return "Hello, $name"
}

suspend fun fetchData(): Result {
    return Result.success()
}

inline fun <T> execute(block: () -> T): T = block()

private fun helper() {}

override fun toString(): String = "test"

operator fun plus(other: MyType): MyType = MyType()

infix fun Int.shl(x: Int): Int = this shl x
`
	p := NewKotlinParser()
	pf, err := p.Parse("Functions.kt", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	if sym, ok := symbolMap["greet"]; !ok {
		t.Error("expected symbol 'greet' not found")
	} else {
		if sym.Kind != "function" {
			t.Errorf("expected greet kind=function, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected greet to be exported (default visibility)")
		}
	}

	if sym, ok := symbolMap["fetchData"]; !ok {
		t.Error("expected symbol 'fetchData' not found")
	} else {
		if sym.Kind != "function" {
			t.Errorf("expected fetchData kind=function, got %q", sym.Kind)
		}
	}

	if sym, ok := symbolMap["execute"]; !ok {
		t.Error("expected symbol 'execute' not found")
	} else {
		if sym.Kind != "function" {
			t.Errorf("expected execute kind=function, got %q", sym.Kind)
		}
	}

	if sym, ok := symbolMap["helper"]; !ok {
		t.Error("expected symbol 'helper' not found")
	} else {
		if sym.Exported {
			t.Error("expected helper to NOT be exported (private)")
		}
	}

	if _, ok := symbolMap["toString"]; !ok {
		t.Error("expected symbol 'toString' not found")
	}

	if _, ok := symbolMap["plus"]; !ok {
		t.Error("expected symbol 'plus' not found")
	}

	if _, ok := symbolMap["shl"]; !ok {
		t.Error("expected symbol 'shl' not found")
	}
}

func TestKotlinParserProperties(t *testing.T) {
	code := `package com.example

val name: String = "test"
var count: Int = 0
const val MAX_SIZE: Int = 100
lateinit var service: Service
private val secret: String = "hidden"
`
	p := NewKotlinParser()
	pf, err := p.Parse("Props.kt", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	if sym, ok := symbolMap["name"]; !ok {
		t.Error("expected symbol 'name' not found")
	} else {
		if sym.Kind != "variable" {
			t.Errorf("expected name kind=variable, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected name to be exported")
		}
	}

	if _, ok := symbolMap["count"]; !ok {
		t.Error("expected symbol 'count' not found")
	}

	if _, ok := symbolMap["MAX_SIZE"]; !ok {
		t.Error("expected symbol 'MAX_SIZE' not found")
	}

	if _, ok := symbolMap["service"]; !ok {
		t.Error("expected symbol 'service' not found")
	}

	if sym, ok := symbolMap["secret"]; !ok {
		t.Error("expected symbol 'secret' not found")
	} else {
		if sym.Exported {
			t.Error("expected secret to NOT be exported (private)")
		}
	}
}

func TestKotlinParserExports(t *testing.T) {
	code := `package com.example

class PublicClass
private class PrivateClass
data class DataModel(val id: Int)
interface Service
fun publicFunction() {}
private fun privateFunction() {}
`
	p := NewKotlinParser()
	pf, err := p.Parse("Exports.kt", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}

	if !exportNames["PublicClass"] {
		t.Error("expected PublicClass to be exported")
	}
	if exportNames["PrivateClass"] {
		t.Error("expected PrivateClass NOT to be exported")
	}
	if !exportNames["DataModel"] {
		t.Error("expected DataModel to be exported")
	}
	if !exportNames["Service"] {
		t.Error("expected Service to be exported")
	}
	if !exportNames["publicFunction"] {
		t.Error("expected publicFunction to be exported")
	}
	if exportNames["privateFunction"] {
		t.Error("expected privateFunction NOT to be exported")
	}
}

func TestKotlinParserBlockComments(t *testing.T) {
	code := `package com.example

/*
class InsideComment
fun alsoInsideComment() {}
*/

class RealClass
`
	p := NewKotlinParser()
	pf, err := p.Parse("Comments.kt", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	if _, ok := symbolMap["InsideComment"]; ok {
		t.Error("should not find symbols inside block comments")
	}
	if _, ok := symbolMap["alsoInsideComment"]; ok {
		t.Error("should not find symbols inside block comments")
	}
	if _, ok := symbolMap["RealClass"]; !ok {
		t.Error("expected symbol 'RealClass' not found")
	}
}

func TestKotlinParserTypeAlias(t *testing.T) {
	code := `package com.example

typealias StringList = List<String>
private typealias InternalMap = Map<String, Any>
`
	p := NewKotlinParser()
	pf, err := p.Parse("Aliases.kt", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	symbolMap := make(map[string]Symbol)
	for _, sym := range pf.Symbols {
		symbolMap[sym.Name] = sym
	}

	if sym, ok := symbolMap["StringList"]; !ok {
		t.Error("expected symbol 'StringList' not found")
	} else {
		if sym.Kind != "type" {
			t.Errorf("expected StringList kind=type, got %q", sym.Kind)
		}
		if !sym.Exported {
			t.Error("expected StringList to be exported")
		}
	}

	if sym, ok := symbolMap["InternalMap"]; !ok {
		t.Error("expected symbol 'InternalMap' not found")
	} else {
		if sym.Exported {
			t.Error("expected InternalMap to NOT be exported (private)")
		}
	}
}

func TestKotlinParserFullFile(t *testing.T) {
	code := `package com.example.app

import java.util.List
import kotlinx.coroutines.launch
import kotlinx.coroutines.CoroutineScope

data class User(val name: String, val email: String)

interface UserRepository {
    suspend fun findById(id: Long): User?
    suspend fun save(user: User)
}

class UserService(private val repo: UserRepository) {
    suspend fun getUser(id: Long): User? {
        return repo.findById(id)
    }

    suspend fun createUser(name: String, email: String): User {
        val user = User(name, email)
        repo.save(user)
        return user
    }
}

object UserFactory {
    fun create(name: String): User = User(name, "$name@example.com")
}
`
	p := NewKotlinParser()
	pf, err := p.Parse("UserService.kt", []byte(code))
	if err != nil {
		t.Fatalf("parsing: %v", err)
	}

	// Verify imports
	if len(pf.Imports) != 3 {
		t.Errorf("expected 3 imports, got %d", len(pf.Imports))
	}

	// Verify symbols exist
	symbolNames := make(map[string]bool)
	for _, sym := range pf.Symbols {
		symbolNames[sym.Name] = true
	}

	for _, expected := range []string{"User", "UserRepository", "UserService", "UserFactory", "getUser", "createUser", "create"} {
		if !symbolNames[expected] {
			t.Errorf("expected symbol %q not found", expected)
		}
	}

	// Verify exports (public by default)
	exportNames := make(map[string]bool)
	for _, exp := range pf.Exports {
		exportNames[exp.Name] = true
	}

	for _, expected := range []string{"User", "UserRepository", "UserService", "UserFactory"} {
		if !exportNames[expected] {
			t.Errorf("expected export %q not found", expected)
		}
	}
}

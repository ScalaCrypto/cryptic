# Agent Guidelines for Cryptic

This document provides coding style and testing guidelines for AI agents working on the Cryptic project.

## Coding Style

Cryptic is written in **Scala 3** and follows modern Scala practices, emphasizing type safety and functional patterns.

### 1. Scala 3 Syntax
- **Indentation-based syntax**: Do not use optional braces for classes, objects, methods, or control structures (e.g., `if`, `for`, `match`).
- **Contextual Abstractions**: Use `given` and `using` instead of Scala 2 style `implicit`.
- **Extensions**: Use `extension` blocks to add methods to existing types, especially for providing DSL-like syntax.
- **Enums**: Use `enum` for ADTs where appropriate.
- **Top-level definitions**: Methods and variables can be defined at the top level of a file within a package.
- **Export**: Use `export` for delegation and composition.

### 2. Formatting
- **Indentation**: Use **2 spaces**.
- **Scalafmt**: Adhere to the configuration in `.scalafmt.conf`.
- **Naming**: 
  - `CamelCase` for types (classes, traits, objects, enums).
  - `camelCase` for members (methods, fields, variables).
  - `F[_]` for higher-kinded effect types.
- **Type Hints**: Provide explicit return types for public methods and complex expressions to improve readability and compilation speed.

### 3. Documentation
- Use **Scaladoc** (KDoc style) for all public APIs.
- Use triple quotes `/** ... */` for docstrings.
- Document type parameters (`@tparam`) and parameters (`@param`).

### 4. Common Patterns
- **Immutability**: Prefer immutable data structures. Use `IArray` (Immutable Array) for performance-critical byte manipulation.
- **Effect Abstraction**: Most core logic is abstracted over an effect type `F[_]` with a `Functor[F]` or similar constraint.
- **Error Handling**: Use `Either`, `Try`, or the effect type `F` to represent failures. Avoid throwing exceptions.

## Testing Guidelines

We use **ScalaTest** for verification.

### 1. Test Location and Naming
- Place tests in `src/test/scala`, mirroring the source package structure.
- Naming convention: `<Subject>Spec.scala` (e.g., `AesSpec.scala`).

### 2. Test Styles
- **AnyFlatSpec**: Use for standard synchronous unit tests.
- **AsyncFlatSpec**: Use for tests involving `Future` or other asynchronous effects.
- **Matchers**: Mix in `org.scalatest.matchers.should.Matchers` for `shouldBe` style assertions.
- **Contexts**: Mix in `TryValues`, `OptionValues`, or `ScalaFutures` as needed.

### 3. Structure
- Use `it should "..." in:` for individual test cases.
- Group tests by subject using `"Subject" should ...` blocks.
- **Indentation**: Tests also follow Scala 3 indentation-based syntax (e.g., `in:` followed by an indented block).

## Project Structure

- `core`: The heart of the library. Contains core traits (`Cryptic`, `Encrypt`, `Decrypt`, `Codec`) and default implementations.
- `cipher-javax`: JCE-based implementations (AES, RSA).
- `cipher-bouncycastle`: BouncyCastle-based implementations (ECIES).
- `cipher-enigma`: A CLI-based Enigma cipher implementation.
- `codec-fst` / `codec-upickle`: Serialization support.
- `cipher-test`: Contains base classes and utilities for testing ciphers.

## SBT Commands

- `sbt compile`: Compile the project.
- `sbt test`: Run all tests.
- `sbt "project core" test`: Run tests for a specific module.
- `sbt scalafmtAll`: Format all files.

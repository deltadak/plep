// Gradle file from https://github.com/PHPirates/kotlin-template-project

import org.gradle.api.plugins.ExtensionAware

import org.junit.platform.gradle.plugin.FiltersExtension
import org.junit.platform.gradle.plugin.EnginesExtension
import org.junit.platform.gradle.plugin.JUnitPlatformExtension
import org.gradle.jvm.tasks.Jar

group = "deltadak"
version = "v2.0.0-beta.7"

// Latest version as of 2018-03-13: JUnit 5.1.0 = Platform 1.1.0 + Jupiter 5.1.0 + Vintage 5.1.0

// JUnit 5
buildscript {
    repositories {
        mavenLocal()
        mavenCentral()
        jcenter()
    }
    dependencies {
        classpath("org.junit.platform:junit-platform-gradle-plugin:1.1.1")
    }
}

apply {
    plugin("org.junit.platform.gradle.plugin")
}

// Kotlin configuration.
plugins {
    application
    kotlin("jvm") version "1.2.31"
    java // Required by at least JUnit.
    // Plugin to build .exe files.
    id("edu.sc.seis.launch4j") version "2.4.3"

    // help/dependencyUpdates checks for dependency updates.
    id("com.github.ben-manes.versions") version "0.16.0"

    // other/useLatestVersions should update version numbers
    // Does not work yet for the GK DSL
//    id("se.patrikerdes.use-latest-versions") version "0.2.0"
}

launch4j {
    mainClassName = "nl.deltadak.plep.MainKt"
    icon = "$projectDir/src/main/resources/icon.ico"
    manifest = "$projectDir/releasing/launch4j/plep.manifest"
}

application {
    mainClassName = "nl.deltadak.plep.Main"
}

dependencies {
    // Plep dependencies
    // JDBC driver for database
    // https://mvnrepository.com/artifact/org.xerial/sqlite-jdbc
    // 'compile' is deprecated, now it is 'api' but that fails to build with
//    Could not find method api() for arguments [org.xerial:sqlite-jdbc:3.18.0] on object of type org.gradle.api.internal.artifacts.dsl.dependencies.DefaultDependencyHandler.
    compile("org.xerial:sqlite-jdbc:3.21.0.1")

    // JNA, used to e.g. make a program pinnable to taskbar.
    compile("net.java.dev.jna:jna:4.5.1")
    compile("net.java.dev.jna:jna-platform:4.5.1")

    // Kotlin
    compile(kotlin("stdlib"))
    // To "prevent strange errors".
    compile(kotlin("reflect"))
    // Kotlin reflection.
    compile(kotlin("test"))
    compile(kotlin("test-junit"))

    // Built-in Kotlin test framework.
    testCompile("io.kotlintest:kotlintest:2.0.7")

    // JUnit 5
    testImplementation("org.junit.jupiter:junit-jupiter-api:5.1.1")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:5.1.1")
    testRuntime("org.junit.platform:junit-platform-console:1.1.1")

    // Kotlintests are not run anyway when using JUnit 5 as well.
    testCompile("io.kotlintest:kotlintest:2.0.7")

    // JavaFX tests using TestFX
    testCompile("org.testfx:testfx-core:4.0.13-alpha")
    testCompile("org.testfx:testfx-junit:4.0.13-alpha")
    // Only needed for headless testing.
//    testCompile("org.testfx:openjfx-monocle:8u76-b04") // jdk-9+181 for Java 9

    // Spek
    testCompile("org.jetbrains.spek:spek-api:1.1.19")
    testRuntime("org.jetbrains.spek:spek-junit-platform-engine:1.1.19")
}

repositories {
    mavenCentral()
    jcenter()
    mavenLocal()
}

/** Build an executable jar. */
val fatJar = task("fatJar", type = Jar::class) {
    baseName = project.name
    manifest {
        attributes["Implementation-Title"] = "Gradle Jar File Example"
        attributes["Implementation-Version"] = version
        attributes["Main-Class"] = "nl.deltadak.plep.Main"
    }
    from(configurations.runtime.map({
        @Suppress("IMPLICIT_CAST_TO_ANY")
        if (it.isDirectory) it else zipTree(it)
    }))
    with(tasks["jar"] as CopySpec)
}

tasks {
    "build" {
        dependsOn(fatJar)
    }
}

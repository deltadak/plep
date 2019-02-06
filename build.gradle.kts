// Gradle file from https://github.com/PHPirates/kotlin-template-project

import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

group = "deltadak"
version = "v1.2.5"

plugins {
    application
    kotlin("jvm") version "1.3.20"
    java // Required by at least JUnit.

    // Plugin to build .exe files.
    id("edu.sc.seis.launch4j") version "2.4.4"

    // Plugin to build fat jars
    id("com.github.johnrengelman.shadow") version "4.0.4"

    // help/dependencyUpdates checks for dependency updates.
    id("com.github.ben-manes.versions") version "0.20.0"

    // help/useLatestVersions updates dependency versions
    id("se.patrikerdes.use-latest-versions") version "0.2.7"

    // Code coverage
    jacoco

    // Upload jacoco coverage reports to coveralls
    id("com.github.kt3k.coveralls") version "2.8.2"
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
    compile("org.xerial:sqlite-jdbc:3.25.2")

    // Database driver (for possible future use with Exposed).
    compile("com.h2database:h2:1.4.197")

    // JNA, used to e.g. make a program pinnable to task bar.
    compile("net.java.dev.jna:jna:5.2.0")
    compile("net.java.dev.jna:jna-platform:5.2.0")

    // Kotlin
    compile(kotlin("stdlib:1.3.11"))

    // To "prevent strange errors".
    compile(kotlin("reflect:1.3.11"))

    // Kotlin reflection.
    compile(kotlin("test"))
    compile(kotlin("test-junit"))

    // Coroutines
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.1.1")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-javafx:1.1.1")

    // Kotlin Exposed SQL DSL
    compile("org.jetbrains.exposed:exposed:0.12.1")
    compile("org.slf4j:slf4j-simple:1.8.0-beta2")

    // JUnit 5
    testImplementation("org.junit.jupiter:junit-jupiter-api:5.4.0-RC2")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:5.4.0-RC2")
    testRuntime("org.junit.platform:junit-platform-console:1.4.0-RC2")

    // Kotlintests are not run anyway when using JUnit 5 as well.
    testCompile("io.kotlintest:kotlintest-core:3.2.1")
    testCompile("io.kotlintest:kotlintest-assertions:3.2.1")
    testCompile("io.kotlintest:kotlintest-runner-junit5:3.2.1")

    // JavaFX tests using TestFX
    testCompile("org.testfx:testfx-core:4.0.15-alpha")
    testCompile("org.testfx:testfx-junit:4.0.15-alpha")
    // Only needed for headless testing.
//    testCompile("org.testfx:openjfx-monocle:8u76-b04") // jdk-9+181 for Java 9

    // Spek
    testCompile("org.jetbrains.spek:spek-api:1.2.1")
    testRuntime("org.jetbrains.spek:spek-junit-platform-engine:1.2.1")
}

repositories {
    mavenCentral()
    jcenter()
    mavenLocal()
}

launch4j {
    mainClassName = "nl.deltadak.plep.MainKt"
    icon = "$projectDir/src/main/resources/plep32.ico"
    manifest = "$projectDir/releasing/Windows/launch4j/plep.manifest"
}

tasks {

    // Configure the shadowJar task.
    "shadowJar"(ShadowJar::class) {
        classifier = ""
    }

    "build" {
        dependsOn(shadowJar)
    }

    // Use the built-in JUnit support of Gradle.
    "test"(Test::class) {
        useJUnitPlatform()
    }

    // Enable xml for coveralls.
    "jacocoTestReport"(JacocoReport::class) {
        reports {
            // To be read by humans
            html.isEnabled = true
            // To be read by Coveralls etc.
            xml.isEnabled = true
            xml.destination = file("$buildDir/reports/jacoco/test/jacocoTestReport.xml")
        }
    }
}

//jacoco {
//    reportsDir = file("$buildDir/reports")
//}

// Gradle file from https://github.com/PHPirates/kotlin-template-project

import org.gradle.api.plugins.ExtensionAware

import org.gradle.jvm.tasks.Jar

group = "deltadak"
version = "v1.2.4"

plugins {
    application
    kotlin("jvm") version "1.3.11"
    java // Required by at least JUnit.

    // Plugin to build .exe files.
    id("edu.sc.seis.launch4j") version "2.4.4"

    // help/dependencyUpdates checks for dependency updates.
    id("com.github.ben-manes.versions") version "0.20.0"

    // help/useLatestVersions updates dependency versions
    id("se.patrikerdes.use-latest-versions") version "0.2.7"

    // Code coverage
    jacoco

    // Upload jacoco coverage reports to coveralls
    id("com.github.kt3k.coveralls") version "2.8.2"
}

launch4j {
    mainClassName = "nl.deltadak.plep.MainKt"
    icon = "$projectDir/src/main/resources/plep32.ico"
    manifest = "$projectDir/releasing/Windows/launch4j/plep.manifest"
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
    compile("net.java.dev.jna:jna:5.1.0")
    compile("net.java.dev.jna:jna-platform:5.1.0")

    // Kotlin
    compile(kotlin("stdlib:1.3.11"))

    // To "prevent strange errors".
    compile(kotlin("reflect:1.3.11"))

    // Kotlin reflection.
    compile(kotlin("test"))
    compile(kotlin("test-junit"))

    // Coroutines
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.1.0-alpha")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-javafx:1.1.0-alpha")

    // Kotlin Exposed SQL DSL
    compile("org.jetbrains.exposed:exposed:0.11.2")
    compile("org.slf4j:slf4j-simple:1.8.0-beta2")


    // JUnit 5
    testImplementation("org.junit.jupiter:junit-jupiter-api:5.3.2")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:5.3.2")
    testRuntime("org.junit.platform:junit-platform-console:1.3.2")

    // Kotlintests are not run anyway when using JUnit 5 as well.
    testCompile("io.kotlintest:kotlintest-core:3.1.11")
    testCompile("io.kotlintest:kotlintest-assertions:3.1.11")
    testCompile("io.kotlintest:kotlintest-runner-junit5:3.1.11")

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

/** Build an executable jar. */
val fatJar = task("fatJar", type = Jar::class) {
    baseName = project.name
    manifest {
        attributes["Implementation-Title"] = "Gradle Jar File"
        attributes["Implementation-Version"] = version
        attributes["Main-Class"] = "nl.deltadak.plep.Main"
    }
    from(configurations.runtime.get().map {
        if (it.isDirectory) it else zipTree(it)
    })
    with(tasks["jar"] as CopySpec)
}

tasks {
    "build" {
        dependsOn(fatJar)
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

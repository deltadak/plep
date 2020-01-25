// Gradle file from https://github.com/PHPirates/kotlin-template-project

import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

group = "deltadak"
version = "v1.2.5"

plugins {
    application
    kotlin("jvm") version "1.3.20"
    java // Required by at least JUnit.

    // Plugin to build .exe files.
    id("edu.sc.seis.launch4j") version "2.4.6"

    // Plugin to build fat jars
    id("com.github.johnrengelman.shadow") version "5.2.0"

    // help/dependencyUpdates checks for dependency updates.
    id("com.github.ben-manes.versions") version "0.27.0"

    // help/useLatestVersions updates dependency versions
    id("se.patrikerdes.use-latest-versions") version "0.2.13"

    // Code coverage
    jacoco

    // Upload jacoco coverage reports to coveralls
    id("com.github.kt3k.coveralls") version "2.9.0"
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
    implementation("org.xerial:sqlite-jdbc:3.30.1")

    // Database driver (for possible future use with Exposed).
    implementation("com.h2database:h2:1.4.200")

    // JNA, used to e.g. make a program pinnable to task bar.
    implementation("net.java.dev.jna:jna:5.5.0")
    implementation("net.java.dev.jna:jna-platform:5.5.0")

    // Kotlin
    implementation(kotlin("stdlib:1.3.11"))

    // To "prevent strange errors".
    implementation(kotlin("reflect:1.3.11"))

    // Kotlin reflection.
    implementation(kotlin("test"))
    implementation(kotlin("test-junit"))

    // Coroutines
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.3.3")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-javafx:1.3.3")

    // Kotlin Exposed SQL DSL
    implementation("org.jetbrains.exposed:exposed:0.17.7")
    implementation("org.slf4j:slf4j-simple:2.0.0-alpha1")

    // JUnit 5
    testImplementation("org.junit.jupiter:junit-jupiter-api:5.6.0")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:5.6.0")
    testRuntimeOnly("org.junit.platform:junit-platform-console:1.6.0")

    // Kotlintests are not run anyway when using JUnit 5 as well.
    testImplementation("io.kotlintest:kotlintest-core:3.4.2")
    testImplementation("io.kotlintest:kotlintest-assertions:3.4.2")
    testImplementation("io.kotlintest:kotlintest-runner-junit5:3.4.2")

    // JavaFX tests using TestFX
    testImplementation("org.testfx:testfx-core:4.0.16-alpha")
    testImplementation("org.testfx:testfx-junit:4.0.16-alpha")
    // Only needed for headless testing.
//    testCompile("org.testfx:openjfx-monocle:8u76-b04") // jdk-9+181 for Java 9

    // Spek
    testImplementation("org.jetbrains.spek:spek-api:1.2.1")
    testImplementation("org.jetbrains.spek:spek-junit-platform-engine:1.2.1")
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

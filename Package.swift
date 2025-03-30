// swift-tools-version: 6.0

import PackageDescription

let package = Package(
    name: "Bliss",
    dependencies: [
        .package(url: "https://github.com/apple/swift-argument-parser.git", from: "1.2.0"),
        .package(
            url: "https://github.com/YusukeHosonuma/SwiftPrettyPrint.git",
            .upToNextMajor(from: "1.2.0")),
    ],
    targets: [
        .executableTarget(
            name: "Bliss",
            dependencies: [
                .product(name: "ArgumentParser", package: "swift-argument-parser"),
                "SwiftPrettyPrint",
            ]
        )
    ]
)

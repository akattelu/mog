const std = @import("std");
const os = @import("builtin").os.tag;

const release_targets: []const std.Target.Query = &.{
    .{ .cpu_arch = .aarch64, .os_tag = .macos },
    .{ .cpu_arch = .aarch64, .os_tag = .linux },
    .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu },
    .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .musl },
    .{ .cpu_arch = .x86_64, .os_tag = .windows },
};

const debug_targets: []const std.Target.Query = &.{
    .{},
};

/// Build entrypoint
pub fn build(b: *std.Build) !void {
    const release_binaries = b.option(bool, "release", "Whether or not to build release binaries (outputs different target architectures)") orelse false;

    // Only output binaries when running `zig build -Drelease=true`
    if (release_binaries) {
        return try release(b);
    }

    // Otherwise, build native, setup tests, setup run command
    const module = b.addModule("mog", .{
        .root_source_file = b.path("src/main.zig"),
        .target = b.standardTargetOptions(.{}),
        .optimize = b.standardOptimizeOption(.{}),
    });

    const exe = b.addExecutable(.{
        .name = "mog",
        .root_module = module,
    });
    b.installArtifact(exe);

    // Create test command
    const test_step = b.step("test", "Run unit tests");
    const unit_tests = b.addTest(.{
        .root_module = module,
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);
    test_step.dependOn(&run_unit_tests.step);

    // Create run command
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Create a new step for installing pre-commit hooks
    // This step has no dependencies
    const precommit_step = b.step("install-git-hooks", "Install git hooks");
    precommit_step.makeFn = (copyGitHooks);
}

/// Build step that copies hooks from .githooks/ to .git/hooks
fn copyGitHooks(_: *std.Build.Step, _: std.Build.Step.MakeOptions) !void {
    const cwd = std.fs.cwd();

    // Ensure .git/hooks directory exists
    cwd.makePath(".git/hooks") catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    // Open source and destination directories
    var githooks_dir = std.fs.cwd().openDir(".githooks", .{ .iterate = true }) catch |err| switch (err) {
        error.FileNotFound => {
            std.log.info("No .githooks directory found, skipping hook installation", .{});
            return;
        },
        else => return err,
    };
    defer githooks_dir.close();

    // Grab handle to hooks dir
    var native_hooks_dir = try cwd.openDir(".git/hooks", .{});
    defer native_hooks_dir.close();

    // Iterate over source files
    var iterator = githooks_dir.iterate();
    while (try iterator.next()) |source_hook| {
        // Copy hook from src to dest
        try githooks_dir.copyFile(source_hook.name, native_hooks_dir, source_hook.name, .{});
        std.log.info("Copied hook from .githooks/{s} to .git/hooks/{s}", .{ source_hook.name, source_hook.name });

        // Open the file and make sure its executable on non-windows systems
        if (os != .windows) {
            var dest_file = try native_hooks_dir.openFile(source_hook.name, .{});
            defer dest_file.close();
            try dest_file.chmod(0o755);
            std.log.info("Set permissions of .githooks/{s} to 755", .{source_hook.name});
        }
    }
}

fn release(b: *std.Build) !void {
    const optimize: std.builtin.OptimizeMode = .ReleaseSafe;
    for (release_targets) |target_query| {
        const module = b.addModule("mog", .{
            .root_source_file = b.path("src/main.zig"),
            .target = b.resolveTargetQuery(target_query),
            .optimize = optimize,
        });

        const exe = b.addExecutable(.{
            .name = "mog",
            .root_module = module,
        });

        const triple = try target_query.zigTriple(b.allocator);
        const target_output = b.addInstallArtifact(exe, .{
            .dest_dir = .{
                .override = .{
                    .custom = triple,
                },
            },
        });

        b.getInstallStep().dependOn(&target_output.step);
    }
}

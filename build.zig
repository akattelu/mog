const std = @import("std");
const os = @import("builtin").os.tag;

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    // const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    // const optimize = b.standardOptimizeOption(.{ .preferred_optimize_mode = .Debug });

    const module = b.addModule("mog", .{ .root_source_file = b.path("src/main.zig"), .target = b.standardTargetOptions(.{}) });

    const exe = b.addExecutable(.{
        .name = "mog",
        .root_module = module,
    });

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{ .root_module = module });
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    // This is where the interesting part begins.
    // As you can see we are re-defining the same
    // executable but we're binding it to a
    // dedicated build step.
    const exe_check = b.addExecutable(.{
        .name = "mog",
        .root_module = module,
    });

    // Any other code to define dependencies would
    // probably be here.

    // These two lines you might want to copy
    // (make sure to rename 'exe_check')
    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&exe_check.step);

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

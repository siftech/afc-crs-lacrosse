{
  inputs.flake-compat.url = "github:edolstra/flake-compat/v1.0.1";
  outputs =
    {
      self,
      flake-compat,
      flake-utils,
      nixpkgs,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        sift-ld = pkgs.writeShellApplication {
          name = "sift-ld.lld";
          runtimeInputs = [
            pkgs.lld_15
            pkgs.llvm_15
            pkgs.llvmPackages_15.bintools
            pkgs.patchelf
          ];
          text = ''
            syms=$(mktemp)
            trap 'rm "$syms"' EXIT

            args=()
            wraps=()
            have_output=0
            next_is_output=0

            for arg in "$@"; do
              if [[ $next_is_output = 1 ]]; then
                have_output=1
                next_is_output=0
                output="$arg"
              fi
              case "$arg" in
              -L*/nix/store/*|/usr/lib/libFuzzingEngine.a)
                echo "dropping flag $arg" >&2
                ;;
              -o)
                next_is_output=1
                args+=("$arg")
                ;;
              --wrap=*)
                wraps+=("''${arg#--wrap=}")
                ;;
              *)
                args+=("$arg")
                if [[ -f "$arg" ]]; then
                  nm -g "$arg" >>"$syms" 2>/dev/null
                fi
                ;;
              esac
            done

            for wrap in "''${wraps[@]}"; do
              if ! grep "__wrap_$wrap" "$syms" >/dev/null; then
                echo "dropping wrap $wrap" >&2
              else
                args+=("--wrap=$wrap")
              fi
            done

            set -ex
            ld.lld \
              -L/usr/local/lib/x86_64-unknown-linux-gnu \
              -L/usr/local/lib \
              -L/usr/lib/gcc/x86_64-linux-gnu/9 \
              -L/usr/lib/x86_64-linux-gnu \
              -L/usr/lib \
              -L/lib/x86_64-linux-gnu \
              -L/lib \
              "''${args[@]}"

            if [[ $have_output != 0 ]]; then
              # We want to prepend, not append.
              rpath=$(patchelf --print-rpath "$output")
              rpath="${pkgs.glibc}/lib:$rpath"
              patchelf --set-rpath "$rpath" "$output"
            fi
          '';
        };
        sift-clang = pkgs.writeShellApplication {
          name = "sift-clang";
          text = ''
              has_fsanitize_fuzzer=0
              has_fsanitize_fuzzer_no_link=0
              for arg in "$@"; do
                      if [[ "$arg" = "-fsanitize=fuzzer" ]]; then
                              has_fsanitize_fuzzer=1
                      elif [[ "$arg" = "-fsanitize=fuzzer-no-link" ]]; then
                              has_fsanitize_fuzzer_no_link=1
                      fi
              done

              extra_args=
              if [[ "$has_fsanitize_fuzzer" = 0 && "$has_fsanitize_fuzzer_no_link" = 0 ]]; then
                      extra_args=-fsanitize=fuzzer-no-link
              fi

              clang $extra_args "$@"
          '';
        };
        sift-clangpp = pkgs.writeShellApplication {
          name = "sift-clang++";
          text = ''
              has_fsanitize_fuzzer=0
              has_fsanitize_fuzzer_no_link=0
              for arg in "$@"; do
                      if [[ "$arg" = "-fsanitize=fuzzer" ]]; then
                              has_fsanitize_fuzzer=1
                      elif [[ "$arg" = "-fsanitize=fuzzer-no-link" ]]; then
                              has_fsanitize_fuzzer_no_link=1
                      fi
              done

              extra_args=
              if [[ "$has_fsanitize_fuzzer" = 0 && "$has_fsanitize_fuzzer_no_link" = 0 ]]; then
                      extra_args=-fsanitize=fuzzer-no-link
              fi

              clang++ $extra_args "$@"
          '';
        };
        aflplusplus = pkgs.aflplusplus.overrideAttrs (old: {
          version = "4.21c";
          src = pkgs.fetchFromGitHub {
            owner = "AFLplusplus";
            repo = "AFLplusplus";
            rev = "v4.21c";
            hash = "sha256-DKwPRxSO+JEJYWLldnfrAYqzwqukNzrbo4R5FzJqzzg=";
          };
          buildPhase = ''
            runHook preBuild

            make all $makeFlags -j$NIX_BUILD_CORES

            runHook postBuild
          '';

          patches = [./aflplusplus_increase_timeouts.patch];


          postInstall = ''
            rm $out/bin/afl-clang $out/bin/afl-clang++
            patchShebangs $out/bin
          '';

          installCheckPhase = ''
            runHook preInstallCheck

            substituteInPlace test/test-llvm.sh \
              --replace '../afl-cmin.bash' '`$out/bin/afl-cmin.bash`'
            patchShebangs .

            echo SKIPPING TESTS

            runHook postInstallCheck
          '';
        });
      in
      rec {
        packages = {
          default = pkgs.symlinkJoin {
            name = "sift-afl-stuff";
            paths = [
              aflplusplus
	      pkgs.execline
              sift-clang
              sift-clangpp
              sift-ld
            ];
          };
          inherit aflplusplus sift-ld;
        };
      }
    );
}

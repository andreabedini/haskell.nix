{
  pkgs = hackage:
    {
      packages = {
        "criterion-measurement".revision = (((hackage."criterion-measurement")."0.1.1.0").revisions).default;
        "criterion-measurement".flags.fast = false;
        "test-framework-hunit".revision = (((hackage."test-framework-hunit")."0.3.0.2").revisions).default;
        "test-framework-hunit".flags.base4 = true;
        "test-framework-hunit".flags.base3 = false;
        "http-client".revision = (((hackage."http-client")."0.6.4").revisions).default;
        "http-client".flags.network-uri = true;
        "cookie".revision = (((hackage."cookie")."0.4.4").revisions).default;
        "void".revision = (((hackage."void")."0.7.3").revisions).default;
        "void".flags.safe = false;
        "semigroupoids".revision = (((hackage."semigroupoids")."5.3.2").revisions).default;
        "semigroupoids".flags.comonad = true;
        "semigroupoids".flags.doctests = true;
        "semigroupoids".flags.unordered-containers = true;
        "semigroupoids".flags.distributive = true;
        "semigroupoids".flags.tagged = true;
        "semigroupoids".flags.containers = true;
        "semigroupoids".flags.contravariant = true;
        "free".revision = (((hackage."free")."5.1.1").revisions).default;
        "Only".revision = (((hackage."Only")."0.1").revisions).default;
        "cereal".revision = (((hackage."cereal")."0.5.8.0").revisions).default;
        "cereal".flags.bytestring-builder = false;
        "exceptions".revision = (((hackage."exceptions")."0.10.2").revisions).default;
        "cryptohash-sha256".revision = (((hackage."cryptohash-sha256")."0.11.101.0").revisions).default;
        "cryptohash-sha256".flags.exe = false;
        "binary".revision = (((hackage."binary")."0.8.6.0").revisions).default;
        "interpolate".revision = (((hackage."interpolate")."0.2.0").revisions).default;
        "generic-random".revision = (((hackage."generic-random")."1.2.0.0").revisions).default;
        "tar".revision = (((hackage."tar")."0.5.1.0").revisions).default;
        "tar".flags.old-time = false;
        "tar".flags.old-bytestring = false;
        "regex-tdfa".revision = (((hackage."regex-tdfa")."1.2.3.2").revisions).default;
        "regex-tdfa".flags.devel = false;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "text-metrics".revision = (((hackage."text-metrics")."0.3.0").revisions).default;
        "text-metrics".flags.dev = false;
        "utf8-string".revision = (((hackage."utf8-string")."1.0.1.1").revisions).default;
        "bifunctors".revision = (((hackage."bifunctors")."5.5.4").revisions).default;
        "bifunctors".flags.semigroups = true;
        "bifunctors".flags.tagged = true;
        "hashing".revision = (((hackage."hashing")."0.1.0.1").revisions).default;
        "extra".revision = (((hackage."extra")."1.6.15").revisions).default;
        "haskeline".revision = (((hackage."haskeline")."0.7.4.3").revisions).default;
        "logict".revision = (((hackage."logict")."0.6.0.3").revisions).default;
        "x509-validation".revision = (((hackage."x509-validation")."1.6.11").revisions).default;
        "fail".revision = (((hackage."fail")."4.9.0.0").revisions).default;
        "dense-linear-algebra".revision = (((hackage."dense-linear-algebra")."0.1.0.0").revisions).default;
        "split".revision = (((hackage."split")."0.2.3.3").revisions).default;
        "data-fix".revision = (((hackage."data-fix")."0.2.0").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "tasty-th".revision = (((hackage."tasty-th")."0.1.7").revisions).default;
        "text-short".revision = (((hackage."text-short")."0.1.2").revisions).default;
        "text-short".flags.asserts = false;
        "microstache".revision = (((hackage."microstache")."1.0.1.1").revisions).default;
        "base-compat-batteries".revision = (((hackage."base-compat-batteries")."0.10.5").revisions).default;
        "hourglass".revision = (((hackage."hourglass")."0.2.12").revisions).default;
        "case-insensitive".revision = (((hackage."case-insensitive")."1.2.1.0").revisions).default;
        "extensible-exceptions".revision = (((hackage."extensible-exceptions")."0.1.1.4").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "tasty-quickcheck".revision = (((hackage."tasty-quickcheck")."0.10.1").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "lifted-async".revision = (((hackage."lifted-async")."0.10.0.4").revisions).default;
        "network-uri".revision = (((hackage."network-uri")."2.6.1.0").revisions).default;
        "asn1-parse".revision = (((hackage."asn1-parse")."0.9.4").revisions).default;
        "regex-base".revision = (((hackage."regex-base")."0.93.2").revisions).default;
        "regex-base".flags.splitbase = true;
        "regex-base".flags.newbase = true;
        "zlib".revision = (((hackage."zlib")."0.6.2").revisions).default;
        "zlib".flags.non-blocking-ffi = false;
        "zlib".flags.pkg-config = false;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "mmorph".revision = (((hackage."mmorph")."1.1.3").revisions).default;
        "js-flot".revision = (((hackage."js-flot")."0.8.3").revisions).default;
        "statistics".revision = (((hackage."statistics")."0.15.0.0").revisions).default;
        "th-expand-syns".revision = (((hackage."th-expand-syns")."0.4.4.0").revisions).default;
        "regex-posix".revision = (((hackage."regex-posix")."0.95.2").revisions).default;
        "regex-posix".flags.splitbase = true;
        "regex-posix".flags.newbase = true;
        "cryptonite".revision = (((hackage."cryptonite")."0.25").revisions).default;
        "cryptonite".flags.support_sse = false;
        "cryptonite".flags.integer-gmp = true;
        "cryptonite".flags.support_rdrand = true;
        "cryptonite".flags.support_aesni = true;
        "cryptonite".flags.support_deepseq = true;
        "cryptonite".flags.support_pclmuldq = false;
        "cryptonite".flags.check_alignment = false;
        "cryptonite".flags.old_toolchain_inliner = false;
        "microlens-aeson".revision = (((hackage."microlens-aeson")."2.3.0.4").revisions).default;
        "clock".revision = (((hackage."clock")."0.8").revisions).default;
        "clock".flags.llvm = false;
        "adjunctions".revision = (((hackage."adjunctions")."4.4").revisions).default;
        "cryptohash-md5".revision = (((hackage."cryptohash-md5")."0.11.100.1").revisions).default;
        "invariant".revision = (((hackage."invariant")."0.5.3").revisions).default;
        "th-orphans".revision = (((hackage."th-orphans")."0.13.7").revisions).default;
        "pem".revision = (((hackage."pem")."0.2.4").revisions).default;
        "megaparsec".revision = (((hackage."megaparsec")."7.0.5").revisions).default;
        "megaparsec".flags.dev = false;
        "syb".revision = (((hackage."syb")."0.7").revisions).default;
        "distributive".revision = (((hackage."distributive")."0.6").revisions).default;
        "distributive".flags.semigroups = true;
        "distributive".flags.tagged = true;
        "asn1-encoding".revision = (((hackage."asn1-encoding")."0.9.5").revisions).default;
        "vector-th-unbox".revision = (((hackage."vector-th-unbox")."0.2.1.6").revisions).default;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.13.1").revisions).default;
        "QuickCheck".flags.templatehaskell = true;
        "scientific".revision = (((hackage."scientific")."0.3.6.2").revisions).default;
        "scientific".flags.integer-simple = false;
        "scientific".flags.bytestring-builder = false;
        "tasty".revision = (((hackage."tasty")."1.2.2").revisions).default;
        "tasty".flags.clock = true;
        "hspec-discover".revision = (((hackage."hspec-discover")."2.7.1").revisions).default;
        "monadlist".revision = (((hackage."monadlist")."0.0.2").revisions).default;
        "half".revision = (((hackage."half")."0.3").revisions).default;
        "parallel".revision = (((hackage."parallel")."3.2.2.0").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "random".revision = (((hackage."random")."1.1").revisions).default;
        "uuid-types".revision = (((hackage."uuid-types")."1.0.3").revisions).default;
        "optparse-applicative".revision = (((hackage."optparse-applicative")."0.14.3.0").revisions).default;
        "network".revision = (((hackage."network")."3.1.0.0").revisions).default;
        "connection".revision = (((hackage."connection")."0.3.0").revisions).default;
        "splitmix".revision = (((hackage."splitmix")."0.0.2").revisions).default;
        "async".revision = (((hackage."async")."2.2.1").revisions).default;
        "async".flags.bench = false;
        "dlist".revision = (((hackage."dlist")."0.8.0.6").revisions).default;
        "conduit".revision = (((hackage."conduit")."1.3.1.1").revisions).default;
        "ref-tf".revision = (((hackage."ref-tf")."0.4.0.1").revisions).default;
        "x509-store".revision = (((hackage."x509-store")."1.6.7").revisions).default;
        "constraints".revision = (((hackage."constraints")."0.11").revisions).default;
        "lens-family-th".revision = (((hackage."lens-family-th")."0.5.0.2").revisions).default;
        "semigroups".revision = (((hackage."semigroups")."0.18.5").revisions).default;
        "semigroups".flags.bytestring = true;
        "semigroups".flags.unordered-containers = true;
        "semigroups".flags.text = true;
        "semigroups".flags.tagged = true;
        "semigroups".flags.containers = true;
        "semigroups".flags.binary = true;
        "semigroups".flags.hashable = true;
        "semigroups".flags.transformers = true;
        "semigroups".flags.deepseq = true;
        "semigroups".flags.bytestring-builder = false;
        "HUnit".revision = (((hackage."HUnit")."1.6.0.0").revisions).default;
        "vector-instances".revision = (((hackage."vector-instances")."3.4").revisions).default;
        "vector-instances".flags.hashable = true;
        "lifted-base".revision = (((hackage."lifted-base")."0.2.3.12").revisions).default;
        "parsec".revision = (((hackage."parsec")."3.1.13.0").revisions).default;
        "th-reify-many".revision = (((hackage."th-reify-many")."0.1.9").revisions).default;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.4").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "directory".revision = (((hackage."directory")."1.3.3.0").revisions).default;
        "yaml".revision = (((hackage."yaml")."0.11.0.0").revisions).default;
        "yaml".flags.no-exe = true;
        "yaml".flags.no-examples = true;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.6.5").revisions).default;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.mtl = true;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.three = false;
        "hpack".revision = (((hackage."hpack")."0.31.2").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.14.0.0").revisions).default;
        "mono-traversable".revision = (((hackage."mono-traversable")."1.0.11.0").revisions).default;
        "vector".revision = (((hackage."vector")."0.12.0.3").revisions).default;
        "vector".flags.unsafechecks = false;
        "vector".flags.internalchecks = false;
        "vector".flags.wall = false;
        "vector".flags.boundschecks = true;
        "call-stack".revision = (((hackage."call-stack")."0.1.0").revisions).default;
        "primitive".revision = (((hackage."primitive")."0.6.4.0").revisions).default;
        "profunctors".revision = (((hackage."profunctors")."5.4").revisions).default;
        "time-locale-compat".revision = (((hackage."time-locale-compat")."0.1.1.5").revisions).default;
        "time-locale-compat".flags.old-locale = false;
        "safe".revision = (((hackage."safe")."0.3.17").revisions).default;
        "blaze-builder".revision = (((hackage."blaze-builder")."0.4.1.0").revisions).default;
        "base-compat".revision = (((hackage."base-compat")."0.10.5").revisions).default;
        "js-jquery".revision = (((hackage."js-jquery")."3.3.1").revisions).default;
        "terminal-size".revision = (((hackage."terminal-size")."0.3.2.1").revisions).default;
        "math-functions".revision = (((hackage."math-functions")."0.3.1.0").revisions).default;
        "math-functions".flags.system-expm1 = false;
        "x509-system".revision = (((hackage."x509-system")."1.6.6").revisions).default;
        "keys".revision = (((hackage."keys")."3.12.2").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.9.1").revisions).default;
        "ansi-terminal".flags.example = false;
        "vector-binary-instances".revision = (((hackage."vector-binary-instances")."0.2.5.1").revisions).default;
        "tagged".revision = (((hackage."tagged")."0.8.6").revisions).default;
        "tagged".flags.transformers = true;
        "tagged".flags.deepseq = true;
        "x509".revision = (((hackage."x509")."1.7.5").revisions).default;
        "cassava".revision = (((hackage."cassava")."0.5.1.0").revisions).default;
        "cassava".flags.bytestring--lt-0_10_4 = false;
        "haskell-src-exts".revision = (((hackage."haskell-src-exts")."1.20.3").revisions).default;
        "lens".revision = (((hackage."lens")."4.17.1").revisions).default;
        "lens".flags.j = false;
        "lens".flags.test-properties = true;
        "lens".flags.old-inline-pragmas = false;
        "lens".flags.test-templates = true;
        "lens".flags.trustworthy = true;
        "lens".flags.test-doctests = true;
        "lens".flags.benchmark-uniplate = false;
        "lens".flags.inlining = true;
        "lens".flags.dump-splices = false;
        "lens".flags.test-hunit = true;
        "lens".flags.safe = false;
        "unliftio-core".revision = (((hackage."unliftio-core")."0.1.2.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.0.1").revisions).default;
        "wl-pprint-annotated".revision = (((hackage."wl-pprint-annotated")."0.1.0.1").revisions).default;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "reflection".revision = (((hackage."reflection")."2.1.4").revisions).default;
        "reflection".flags.slow = false;
        "reflection".flags.template-haskell = true;
        "these".revision = (((hackage."these")."0.8").revisions).default;
        "regex-tdfa-text".revision = (((hackage."regex-tdfa-text")."1.0.0.3").revisions).default;
        "dependent-sum".revision = (((hackage."dependent-sum")."0.5").revisions).default;
        "socks".revision = (((hackage."socks")."0.6.0").revisions).default;
        "streaming-commons".revision = (((hackage."streaming-commons")."0.2.1.0").revisions).default;
        "streaming-commons".flags.use-bytestring-builder = false;
        "haskell-lexer".revision = (((hackage."haskell-lexer")."1.0.2").revisions).default;
        "monad-par".revision = (((hackage."monad-par")."0.3.4.8").revisions).default;
        "monad-par".flags.newgeneric = false;
        "monad-par".flags.chaselev = false;
        "lens-family".revision = (((hackage."lens-family")."1.2.3").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.8.2").revisions).default;
        "ansi-wl-pprint".revision = (((hackage."ansi-wl-pprint")."0.6.9").revisions).default;
        "ansi-wl-pprint".flags.example = false;
        "mwc-random".revision = (((hackage."mwc-random")."0.14.0.0").revisions).default;
        "basement".revision = (((hackage."basement")."0.0.10").revisions).default;
        "test-framework".revision = (((hackage."test-framework")."0.8.2.0").revisions).default;
        "cryptohash-sha1".revision = (((hackage."cryptohash-sha1")."0.11.100.1").revisions).default;
        "serialise".revision = (((hackage."serialise")."0.2.1.0").revisions).default;
        "serialise".flags.newtime15 = true;
        "hostname".revision = (((hackage."hostname")."1.0").revisions).default;
        "old-locale".revision = (((hackage."old-locale")."1.0.0.7").revisions).default;
        "wcwidth".revision = (((hackage."wcwidth")."0.0.2").revisions).default;
        "wcwidth".flags.split-base = true;
        "wcwidth".flags.cli = false;
        "StateVar".revision = (((hackage."StateVar")."1.1.1.1").revisions).default;
        "mime-types".revision = (((hackage."mime-types")."0.1.0.9").revisions).default;
        "http-client-tls".revision = (((hackage."http-client-tls")."0.3.5.3").revisions).default;
        "contravariant".revision = (((hackage."contravariant")."1.5.1").revisions).default;
        "contravariant".flags.semigroups = true;
        "contravariant".flags.tagged = true;
        "contravariant".flags.statevar = true;
        "pointed".revision = (((hackage."pointed")."5.0.1").revisions).default;
        "pointed".flags.semigroupoids = true;
        "pointed".flags.stm = true;
        "pointed".flags.comonad = true;
        "pointed".flags.unordered-containers = true;
        "pointed".flags.kan-extensions = true;
        "pointed".flags.semigroups = true;
        "pointed".flags.tagged = true;
        "pointed".flags.containers = true;
        "pointed".flags.transformers = true;
        "parser-combinators".revision = (((hackage."parser-combinators")."1.0.3").revisions).default;
        "parser-combinators".flags.dev = false;
        "deriving-compat".revision = (((hackage."deriving-compat")."0.5.6").revisions).default;
        "deriving-compat".flags.base-4-9 = true;
        "deriving-compat".flags.template-haskell-2-11 = true;
        "deriving-compat".flags.new-functor-classes = true;
        "text".revision = (((hackage."text")."1.2.3.1").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."2.4.0.1").revisions).default;
        "assoc".revision = (((hackage."assoc")."1").revisions).default;
        "Diff".revision = (((hackage."Diff")."0.3.4").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.10.0").revisions).default;
        "unordered-containers".flags.debug = false;
        "base".revision = (((hackage."base")."4.12.0.0").revisions).default;
        "abstract-deque".revision = (((hackage."abstract-deque")."0.3").revisions).default;
        "abstract-deque".flags.usecas = false;
        "tasty-hedgehog".revision = (((hackage."tasty-hedgehog")."1.0.0.0").revisions).default;
        "comonad".revision = (((hackage."comonad")."5.0.5").revisions).default;
        "comonad".flags.distributive = true;
        "comonad".flags.test-doctests = true;
        "comonad".flags.containers = true;
        "time".revision = (((hackage."time")."1.8.0.2").revisions).default;
        "data-default-class".revision = (((hackage."data-default-class")."0.1.2.0").revisions).default;
        "terminfo".revision = (((hackage."terminfo")."0.4.1.2").revisions).default;
        "base16-bytestring".revision = (((hackage."base16-bytestring")."0.1.1.6").revisions).default;
        "vector-algorithms".revision = (((hackage."vector-algorithms")."0.8.0.1").revisions).default;
        "vector-algorithms".flags.unsafechecks = false;
        "vector-algorithms".flags.internalchecks = false;
        "vector-algorithms".flags.llvm = false;
        "vector-algorithms".flags.boundschecks = true;
        "vector-algorithms".flags.bench = true;
        "vector-algorithms".flags.properties = true;
        "prettyprinter".revision = (((hackage."prettyprinter")."1.2.1").revisions).default;
        "prettyprinter".flags.buildreadme = false;
        "cryptohash-sha512".revision = (((hackage."cryptohash-sha512")."0.11.100.1").revisions).default;
        "tasty-hunit".revision = (((hackage."tasty-hunit")."0.10.0.2").revisions).default;
        "pretty-show".revision = (((hackage."pretty-show")."1.8.2").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.2.7.0").revisions).default;
        "hashable".flags.sse2 = true;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.sse41 = false;
        "hashable".flags.examples = false;
        "attoparsec".revision = (((hackage."attoparsec")."0.13.2.2").revisions).default;
        "attoparsec".flags.developer = false;
        "infer-license".revision = (((hackage."infer-license")."0.2.0").revisions).default;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        "transformers-base".revision = (((hackage."transformers-base")."0.4.5.2").revisions).default;
        "transformers-base".flags.orphaninstances = true;
        "happy".revision = (((hackage."happy")."1.19.10").revisions).default;
        "happy".flags.small_base = true;
        "criterion".revision = (((hackage."criterion")."1.5.5.0").revisions).default;
        "criterion".flags.embed-data-files = false;
        "criterion".flags.fast = false;
        "monad-par-extras".revision = (((hackage."monad-par-extras")."0.3.3").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "code-page".revision = (((hackage."code-page")."0.2").revisions).default;
        "asn1-types".revision = (((hackage."asn1-types")."0.3.2").revisions).default;
        "unbounded-delays".revision = (((hackage."unbounded-delays")."0.1.1.0").revisions).default;
        "hedgehog".revision = (((hackage."hedgehog")."1.0").revisions).default;
        "cborg".revision = (((hackage."cborg")."0.2.1.0").revisions).default;
        "cborg".flags.optimize-gmp = true;
        "monad-control".revision = (((hackage."monad-control")."1.0.2.3").revisions).default;
        "process".revision = (((hackage."process")."1.6.5.0").revisions).default;
        "tls".revision = (((hackage."tls")."1.4.1").revisions).default;
        "tls".flags.compat = true;
        "tls".flags.network = true;
        "tls".flags.hans = false;
        "kan-extensions".revision = (((hackage."kan-extensions")."5.2").revisions).default;
        "th-lift".revision = (((hackage."th-lift")."0.8.0.1").revisions).default;
        "libyaml".revision = (((hackage."libyaml")."0.1.1.0").revisions).default;
        "libyaml".flags.system-libyaml = false;
        "libyaml".flags.no-unicode = false;
        "resourcet".revision = (((hackage."resourcet")."1.2.2").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "cabal-doctest".revision = (((hackage."cabal-doctest")."1.0.6").revisions).default;
        "Glob".revision = (((hackage."Glob")."0.10.0").revisions).default;
        "microlens".revision = (((hackage."microlens")."0.4.10").revisions).default;
        "aeson".revision = (((hackage."aeson")."1.4.3.0").revisions).default;
        "aeson".flags.cffi = false;
        "aeson".flags.fast = false;
        "aeson".flags.bytestring-builder = false;
        "aeson".flags.developer = false;
        "abstract-par".revision = (((hackage."abstract-par")."0.3.3").revisions).default;
        "http-types".revision = (((hackage."http-types")."0.12.3").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.6.5").revisions).default;
        "th-lift-instances".revision = (((hackage."th-lift-instances")."0.1.13").revisions).default;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.1").revisions).default;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.3.1.0").revisions).default;
        "memory".revision = (((hackage."memory")."0.14.18").revisions).default;
        "memory".flags.support_bytestring = true;
        "memory".flags.support_basement = true;
        "memory".flags.support_foundation = true;
        "memory".flags.support_deepseq = true;
        "concurrent-output".revision = (((hackage."concurrent-output")."1.10.10").revisions).default;
        "array".revision = (((hackage."array")."0.5.3.0").revisions).default;
        "repline".revision = (((hackage."repline")."0.2.1.0").revisions).default;
        "xml".revision = (((hackage."xml")."1.3.14").revisions).default;
        "lens-family-core".revision = (((hackage."lens-family-core")."1.2.3").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.6.5";
        nix-name = "ghc865";
        packages = {
          "binary" = "0.8.6.0";
          "ghc-prim" = "0.5.3";
          "haskeline" = "0.7.4.3";
          "stm" = "2.5.0.0";
          "unix" = "2.7.2.2";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "deepseq" = "1.4.4.0";
          "parsec" = "3.1.13.0";
          "directory" = "1.3.3.0";
          "template-haskell" = "2.14.0.0";
          "containers" = "0.6.0.1";
          "bytestring" = "0.10.8.2";
          "text" = "1.2.3.1";
          "Cabal" = "2.4.0.1";
          "base" = "4.12.0.0";
          "time" = "1.8.0.2";
          "terminfo" = "0.4.1.2";
          "transformers" = "0.5.6.2";
          "filepath" = "1.4.2.1";
          "process" = "1.6.5.0";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.6.5";
          "array" = "0.5.3.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        nix-tools = ./.plan.nix/nix-tools.nix;
        hackage-db = ./.plan.nix/hackage-db.nix;
        hnix = ./.plan.nix/hnix.nix;
        haskell-src-meta = ./.plan.nix/haskell-src-meta.nix;
        };
      };
  }
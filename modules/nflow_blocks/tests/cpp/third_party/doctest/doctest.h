// Minimal doctest-compatible single-header subset for the NFlow C++ tests.
// It implements only the API used by this local test runner.
#pragma once

#include <cmath>
#include <cstdlib>
#include <exception>
#include <functional>
#include <iostream>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

namespace doctest {

class Approx
{
public:
    explicit Approx(double value) : value_(value) { }

    Approx&
    epsilon(double value)
    {
        epsilon_ = value;
        return *this;
    }

    Approx&
    scale(double value)
    {
        scale_ = value;
        return *this;
    }

    bool
    compare(double other) const
    {
        return std::fabs(other - value_)
            <= epsilon_ * (scale_ + std::max(std::fabs(other), std::fabs(value_)));
    }

    friend bool
    operator==(double lhs, const Approx& rhs)
    {
        return rhs.compare(lhs);
    }

    friend bool
    operator==(const Approx& lhs, double rhs)
    {
        return lhs.compare(rhs);
    }

    friend bool
    operator!=(double lhs, const Approx& rhs)
    {
        return !rhs.compare(lhs);
    }

    friend bool
    operator!=(const Approx& lhs, double rhs)
    {
        return !lhs.compare(rhs);
    }

private:
    double value_;
    double epsilon_ = 1e-12;
    double scale_ = 1.0;
};

struct TestCase
{
    const char* name;
    void (*fn)();
};

inline std::vector<TestCase>&
registry()
{
    static std::vector<TestCase> tests;
    return tests;
}

struct Register
{
    Register(const char* name, void (*fn)()) { registry().push_back({ name, fn }); }
};

struct TestFailure : std::exception
{ };

inline int&
failureCount()
{
    static int value = 0;
    return value;
}

inline void
check(bool ok, const char* expr, const char* file, int line, bool require)
{
    if (ok) {
        return;
    }
    ++failureCount();
    std::cerr << file << ":" << line << ": " << (require ? "REQUIRE" : "CHECK")
              << " failed: " << expr << std::endl;
    if (require) {
        throw TestFailure();
    }
}

class Context
{
public:
    Context() = default;

    Context(int argc, char** argv)
    {
        for (int i = 1; i < argc; ++i) {
            const std::string arg = argv[i] ? argv[i] : "";
            if (arg == "--verbose" || arg == "-v") {
                verbose_ = true;
            }
        }
    }

    int
    run()
    {
        int failedTests = 0;
        std::cout << "[doctest] running " << registry().size() << " test case(s)" << std::endl;
        for (const auto& tc : registry()) {
            if (verbose_) {
                std::cout << "[doctest] RUN: " << tc.name << std::endl;
            }
            const int before = failureCount();
            try {
                tc.fn();
            } catch (const TestFailure&) {
            } catch (const std::exception& ex) {
                ++failureCount();
                std::cerr << "[doctest] unexpected exception in " << tc.name << ": " << ex.what()
                          << std::endl;
            } catch (...) {
                ++failureCount();
                std::cerr << "[doctest] unexpected non-standard exception in " << tc.name
                          << std::endl;
            }
            if (failureCount() != before) {
                ++failedTests;
                std::cerr << "[doctest] FAILED: " << tc.name << std::endl;
            } else if (verbose_) {
                std::cout << "[doctest] OK: " << tc.name << std::endl;
            }
        }
        std::cout << "[doctest] failures: " << failureCount() << std::endl;
        return failureCount() == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
    }

private:
    bool verbose_ = false;
};

} // namespace doctest

#define DOCTEST_DETAIL_CAT_IMPL(s1, s2) s1##s2
#define DOCTEST_DETAIL_CAT(s1, s2) DOCTEST_DETAIL_CAT_IMPL(s1, s2)

#define TEST_CASE(name)                                                                            \
    static void DOCTEST_DETAIL_CAT(doctest_test_, __LINE__)();                                     \
    static ::doctest::Register DOCTEST_DETAIL_CAT(doctest_reg_, __LINE__)(                         \
        name, &DOCTEST_DETAIL_CAT(doctest_test_, __LINE__));                                       \
    static void DOCTEST_DETAIL_CAT(doctest_test_, __LINE__)()

#define CHECK(expr) ::doctest::check(static_cast<bool>(expr), #expr, __FILE__, __LINE__, false)
#define CHECK_FALSE(expr)                                                                          \
    ::doctest::check(!static_cast<bool>(expr), #expr, __FILE__, __LINE__, false)
#define REQUIRE(expr) ::doctest::check(static_cast<bool>(expr), #expr, __FILE__, __LINE__, true)
#define REQUIRE_FALSE(expr)                                                                        \
    ::doctest::check(!static_cast<bool>(expr), #expr, __FILE__, __LINE__, true)

#ifdef DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
int
main(int argc, char** argv)
{
    ::doctest::Context context(argc, argv);
    return context.run();
}
#endif

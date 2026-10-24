#include <detail/serialization/serialisation_helpers.hpp>
#include <detail/serialization/parsers.hpp>

#include <limits>
#include <cassert>

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wduplicate-enum"
#pragma clang diagnostic ignored "-Wswitch-enum"
#endif
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#if defined(__clang__)
#pragma clang diagnostic pop
#endif

namespace xlnt {
namespace detail {

    std::string
    serialise(double d)
    {
        return fmt::format("{}", d);
    }

    double
    deserialise(const std::string& s, size_t* len_converted)
    {
        assert(!s.empty());
        double d = std::numeric_limits<double>::quiet_NaN();
        detail::parse(s, d, len_converted);
        return d;
    }

    double
    deserialise(const char* s, const char** end)
    {
        assert(s != nullptr);
        double d = std::numeric_limits<double>::quiet_NaN();
        detail::parse(s, d, end);
        return d;
    }

} // namespace detail
} // namespace xlnt

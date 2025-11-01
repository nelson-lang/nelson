#pragma once

#include "./IEnvManager.hpp"
#include "./EnvManager.hpp"

namespace cppenv::type {

using OptString = std::optional<std::string>;
using OptInt = std::optional<int>;
using OptFloat = std::optional<float>;
using OptDouble = std::optional<double>;
using OptUInt = std::optional<unsigned int>;
using OptSize_t = std::optional<size_t>;
using OptChar = std::optional<char>;
using OptBool = std::optional<bool>;

}

namespace cppenv::convert_type {

bool
string_to_bool(const std::optional<std::string>& opt_str, bool default_value = false)
{

    if (!opt_str.has_value()) {
        return default_value;
    }

    std::string str = *opt_str;
    std::transform(
        str.begin(), str.end(), str.begin(), [](unsigned char c) { return std::tolower(c); });

    if (str == "true" || str == "1") {
        return true;
    }
    if (str == "false" || str == "0") {
        return false;
    }

    throw std::invalid_argument("Invalid boolean string : " + *opt_str);
}

}

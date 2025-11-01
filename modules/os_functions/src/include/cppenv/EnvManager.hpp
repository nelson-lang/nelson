#pragma once

#include "./IEnvManager.hpp"

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <optional>
#include <type_traits>
#include <algorithm>
#include <utility>
#include <string>
#include <filesystem> // added

namespace cppenv {

class EnvManager : public IEnvManager
{
public:
    bool
    load_from_file(const std::filesystem::path& filename) override
    {
#ifdef _MSC_VER
        std::ifstream file(filename.wstring(), std::ios::in | std::ios::binary);
#else
        std::ifstream file(filename.string());
#endif

        if (!file.is_open()) {
            return false;
        }

        std::string line;

        while (std::getline(file, line)) {

            line = trim(line);

            if (line.empty() || line[0] == '#')
                continue;

            size_t equal_pos = line.find('=');

            if (equal_pos != std::string::npos) {

                std::string key = trim(line.substr(0, equal_pos));

                std::string value = trim(line.substr(equal_pos + 1));

                value = remove_inline_comment(value);
                value = strip_quotes(value);

                // update existing key or append to preserve order
                auto it = std::find_if(env_vars.begin(), env_vars.end(),
                    [&](const auto& kv) { return kv.first == key; });
                if (it != env_vars.end()) {
                    it->second = value;
                } else {
                    env_vars.emplace_back(key, value);
                }
            }
        }

        file.close();

        return true;
    }

    std::optional<std::string>
    get_value(const std::string& key) const override
    {

        auto it = std::find_if(
            env_vars.cbegin(), env_vars.cend(), [&](const auto& kv) { return kv.first == key; });

        if (it != env_vars.cend()) {

            return it->second;
        }

        return std::nullopt; // Returns an empty value if the key does not exist
    }

    std::vector<std::string>
    names() const
    {
        std::vector<std::string> keys;
        keys.reserve(env_vars.size());

        for (const auto& kv : env_vars) {
            keys.push_back(kv.first);
        }

        return keys;
    }

    std::optional<std::string>
    operator[](const std::string& key) const
    {
        return get_value(key);
    }

    template <typename T>
    std::optional<T>
    operator[](const char* key) const
    {
        return get_value_as<T>(std::string(key));
    }

    /*
    template<typename T, typename std::enable_if<!std::is_same_v<T, std::string>, int>::type = 0>
        std::optional<T> operator[](const std::string& key) const {
        return get_value_as<T>(key);
    }*/

    template <typename T>
    std::optional<T>
    get_value_as(const std::string& key) const
    {

        auto it = std::find_if(
            env_vars.cbegin(), env_vars.cend(), [&](const auto& kv) { return kv.first == key; });

        if (it == env_vars.cend()) {
            return std::nullopt;
        }

        std::istringstream iss(it->second);
        T value;

        if constexpr (std::is_same<T, bool>::value) {

            std::string lower_val = it->second;

            std::transform(lower_val.begin(), lower_val.end(), lower_val.begin(), ::tolower);

            if (lower_val == "true" || lower_val == "1") {
                return true;
            }

            if (lower_val == "false" || lower_val == "0") {
                return false;
            }

            return std::nullopt;

        } else if constexpr (std::is_same<T, std::string>::value) {

            return it->second;

        } else {

            iss >> value;

            if (iss.fail() || !iss.eof()) {
                return std::nullopt;
            }

            return value;
        }
    }

private:
    std::vector<std::pair<std::string, std::string>> env_vars;

    static std::string
    trim(const std::string& str)
    {

        size_t start = str.find_first_not_of(" \t\r\n");
        size_t end = str.find_last_not_of(" \t\r\n");

        return (start == std::string::npos || end == std::string::npos)
            ? ""
            : str.substr(start, end - start + 1);
    }

    static std::string
    remove_inline_comment(const std::string& value)
    {

        bool in_quotes = false;
        std::string cleaned;

        for (size_t i = 0; i < value.size(); ++i) {
            char c = value[i];

            if (c == '"')
                in_quotes = !in_quotes;

            if (!in_quotes && c == '#')
                break;

            cleaned += c;
        }

        return trim(cleaned);
    }

    static std::string
    strip_quotes(const std::string& value)
    {

        if (value.size() >= 2 && (value.front() == '"' && value.back() == '"')) {
            return value.substr(1, value.size() - 2);
        }

        return value;
    }
};

}

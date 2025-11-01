#pragma once
#include <string>
#include <optional>
#include <filesystem>

namespace cppenv {

class IEnvManager
{
public:
    virtual ~IEnvManager() = default;

    virtual bool
    load_from_file(const std::filesystem::path& filename)
        = 0;

    virtual std::optional<std::string>
    get_value(const std::string& key) const = 0;
};

}

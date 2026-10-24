// Copyright (c) 2014-2022 Thomas Fussell
// Copyright (c) 2024-2026 xlnt-community
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE
//
// @license: http://www.opensource.org/licenses/mit-license.php
// @author: see AUTHORS file

#include <xlnt/packaging/uri.hpp>

#include <algorithm>
#include <cctype>
#include <limits>
#include <string>
#include <vector>

namespace xlnt {
namespace {
    bool
    starts_with(const std::string& s, const char* prefix)
    {
        const std::size_t n = std::char_traits<char>::length(prefix);
        return s.size() >= n && s.compare(0, n, prefix) == 0;
    }

    std::string
    lower_copy(std::string s)
    {
        for (char& c : s) {
            c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
        }
        return s;
    }

    bool
    is_scheme_char(char c)
    {
        return std::isalpha(static_cast<unsigned char>(c))
            || std::isdigit(static_cast<unsigned char>(c)) || c == '+' || c == '-' || c == '.';
    }

    bool
    looks_like_scheme(const std::string& s)
    {
        if (s.empty() || !std::isalpha(static_cast<unsigned char>(s[0]))) {
            return false;
        }

        for (std::size_t i = 1; i < s.size(); ++i) {
            char c = s[i];
            if (c == ':') {
                return true;
            }
            if (!is_scheme_char(c)) {
                return false;
            }
        }
        return false;
    }

    bool
    parse_port(const std::string& s, std::size_t& port)
    {
        if (s.empty()) {
            return false;
        }

        std::size_t value = 0;
        for (char c : s) {
            if (!std::isdigit(static_cast<unsigned char>(c))) {
                return false;
            }

            const std::size_t digit = static_cast<std::size_t>(c - '0');
            if (value > (std::numeric_limits<std::size_t>::max() - digit) / 10) {
                return false;
            }

            value = value * 10 + digit;
        }

        port = value;
        return true;
    }

    struct parsed_authority
    {
        bool present = false;
        bool authentication = false;
        bool port_present = false;
        std::string username;
        std::string password;
        std::string host;
        std::size_t port = 0;
        bool ipv6 = false;
    };

    parsed_authority
    parse_authority(const std::string& authority)
    {
        parsed_authority result;
        result.present = true;

        if (authority.empty()) {
            return result;
        }

        std::string hostport = authority;

        const std::size_t at = authority.rfind('@');
        if (at != std::string::npos) {
            result.authentication = true;
            const std::string userinfo = authority.substr(0, at);
            hostport = authority.substr(at + 1);

            const std::size_t colon = userinfo.find(':');
            if (colon != std::string::npos) {
                result.username = userinfo.substr(0, colon);
                result.password = userinfo.substr(colon + 1);
            } else {
                result.username = userinfo;
            }
        }

        if (!hostport.empty() && hostport.front() == '[') {
            result.ipv6 = true;
            const std::size_t close = hostport.find(']');
            if (close != std::string::npos) {
                result.host = hostport.substr(1, close - 1);
                if (close + 1 < hostport.size() && hostport[close + 1] == ':') {
                    std::size_t port = 0;
                    if (parse_port(hostport.substr(close + 2), port)) {
                        result.port_present = true;
                        result.port = port;
                    }
                }
            } else {
                result.host = hostport;
            }
        } else {
            const std::size_t colon = hostport.rfind(':');
            if (colon != std::string::npos && hostport.find(':') == colon) {
                std::size_t port = 0;
                if (parse_port(hostport.substr(colon + 1), port)) {
                    result.host = hostport.substr(0, colon);
                    result.port_present = true;
                    result.port = port;
                } else {
                    result.host = hostport;
                }
            } else {
                result.host = hostport;
            }
        }

        return result;
    }

    std::string
    remove_dot_segments(std::string path)
    {
        std::string output;
        while (!path.empty()) {
            if (path.compare(0, 3, "../") == 0) {
                path.erase(0, 3);
            } else if (path.compare(0, 2, "./") == 0) {
                path.erase(0, 2);
            } else if (path.compare(0, 3, "/./") == 0) {
                path.replace(0, 3, "/");
            } else if (path == "/.") {
                path = "/";
            } else if (path.compare(0, 4, "/../") == 0) {
                path.replace(0, 4, "/");
                std::size_t slash = output.find_last_of('/');
                if (slash != std::string::npos) {
                    output.erase(slash);
                } else {
                    output.clear();
                }
            } else if (path == "/..") {
                path = "/";
                std::size_t slash = output.find_last_of('/');
                if (slash != std::string::npos) {
                    output.erase(slash);
                } else {
                    output.clear();
                }
            } else if (path == "." || path == "..") {
                path.clear();
            } else {
                std::size_t next = path.find('/', 1);
                if (next == std::string::npos) {
                    output += path;
                    path.clear();
                } else {
                    output += path.substr(0, next);
                    path.erase(0, next);
                }
            }
        }
        return output;
    }

    std::string
    merge_paths(
        const std::string& base_path, const std::string& relative_path, bool base_has_authority)
    {
        if (base_has_authority && base_path.empty()) {
            return "/" + relative_path;
        }

        const std::size_t slash = base_path.find_last_of('/');
        if (slash == std::string::npos) {
            return relative_path;
        }

        return base_path.substr(0, slash + 1) + relative_path;
    }

    struct parsed_uri
    {
        bool absolute = false;
        bool has_authority = false;
        bool has_authentication = false;
        bool has_port = false;
        bool has_query = false;
        bool has_fragment = false;
        bool ipv6 = false;

        std::string scheme;
        std::string username;
        std::string password;
        std::string host;
        std::size_t port = 0;
        std::string query;
        std::string fragment;
        std::string path;
    };

    parsed_uri
    parse_uri_string(const std::string& uri_string)
    {
        parsed_uri parsed;
        std::string work = uri_string;

        const std::size_t fragment_pos = work.find('#');
        if (fragment_pos != std::string::npos) {
            parsed.fragment = work.substr(fragment_pos + 1);
            parsed.has_fragment = true;
            work = work.substr(0, fragment_pos);
        }

        const std::size_t query_pos = work.find('?');
        if (query_pos != std::string::npos) {
            parsed.query = work.substr(query_pos + 1);
            parsed.has_query = true;
            work = work.substr(0, query_pos);
        }

        const std::size_t first_special = work.find_first_of("/?#");
        const std::string maybe_scheme
            = (first_special == std::string::npos) ? work : work.substr(0, first_special);

        std::size_t scheme_pos = std::string::npos;
        if (looks_like_scheme(maybe_scheme)) {
            scheme_pos = maybe_scheme.find(':');
        }

        std::string rest = work;
        if (scheme_pos != std::string::npos) {
            parsed.scheme = work.substr(0, scheme_pos);
            parsed.absolute = true;
            rest = work.substr(scheme_pos + 1);
        }

        if (starts_with(rest, "//")) {
            parsed.has_authority = true;
            const std::size_t authority_begin = 2;
            const std::size_t authority_end = rest.find_first_of("/?", authority_begin);

            const std::string authority = rest.substr(authority_begin,
                authority_end == std::string::npos ? std::string::npos
                                                   : authority_end - authority_begin);

            rest
                = (authority_end == std::string::npos) ? std::string() : rest.substr(authority_end);

            const parsed_authority auth = parse_authority(authority);
            parsed.ipv6 = auth.ipv6;
            parsed.has_authentication = auth.authentication;
            parsed.has_port = auth.port_present;
            parsed.username = auth.username;
            parsed.password = auth.password;
            parsed.host = auth.host;
            parsed.port = auth.port;
        }

        parsed.path = rest;
        return parsed;
    }

    std::string
    relative_path_from_base(
        const std::string& base_path, const std::string& target_path, bool base_has_authority)
    {
        if (target_path == base_path) {
            return std::string();
        }

        std::string base_dir = base_path;
        if (base_dir.empty()) {
            if (base_has_authority) {
                base_dir = "/";
            }
        } else if (base_dir.back() != '/') {
            const std::size_t slash = base_dir.find_last_of('/');
            if (slash != std::string::npos) {
                base_dir.erase(slash + 1);
            } else {
                base_dir.clear();
            }
        }

        auto split = [](const std::string& p) {
            std::vector<std::string> out;
            std::size_t start = 0;
            while (start <= p.size()) {
                std::size_t pos = p.find('/', start);
                if (pos == std::string::npos) {
                    out.push_back(p.substr(start));
                    break;
                }
                out.push_back(p.substr(start, pos - start));
                start = pos + 1;
                if (start == p.size()) {
                    out.push_back("");
                    break;
                }
            }
            return out;
        };

        const std::vector<std::string> base_segments = split(base_dir);
        const std::vector<std::string> target_segments = split(target_path);

        std::size_t common = 0;
        while (common < base_segments.size() && common < target_segments.size()
            && base_segments[common] == target_segments[common]) {
            ++common;
        }

        std::vector<std::string> result_segments;

        for (std::size_t i = common; i < base_segments.size(); ++i) {
            if (!base_segments[i].empty()) {
                result_segments.push_back("..");
            }
        }

        for (std::size_t i = common; i < target_segments.size(); ++i) {
            result_segments.push_back(target_segments[i]);
        }

        std::string result;
        for (std::size_t i = 0; i < result_segments.size(); ++i) {
            if (i != 0) {
                result.push_back('/');
            }
            result += result_segments[i];
        }

        return result;
    }

    std::string
    normalize_for_compare(const std::string& path)
    {
        return remove_dot_segments(path);
    }
} // namespace

uri::uri() = default;

uri::uri(const std::string& uri_string)
{
    const parsed_uri parsed = parse_uri_string(uri_string);

    absolute_ = parsed.absolute;
    has_authority_ = parsed.has_authority;
    ipv6_ = parsed.ipv6;
    has_authentication_ = parsed.has_authentication;
    has_port_ = parsed.has_port;
    has_query_ = parsed.has_query;
    has_fragment_ = parsed.has_fragment;

    scheme_ = parsed.scheme;
    username_ = parsed.username;
    password_ = parsed.password;
    host_ = parsed.host;
    port_ = parsed.port;
    query_ = parsed.query;
    fragment_ = parsed.fragment;
    path_ = xlnt::path(parsed.path);
}

uri::uri(const uri& base, const uri& relative)
{
    if (relative.absolute_) {
        *this = relative;
        return;
    }

    absolute_ = base.absolute_;
    scheme_ = base.scheme_;

    if (relative.has_authority_) {
        has_authority_ = true;
        has_authentication_ = relative.has_authentication_;
        has_port_ = relative.has_port_;
        username_ = relative.username_;
        password_ = relative.password_;
        host_ = relative.host_;
        port_ = relative.port_;
        path_ = xlnt::path(remove_dot_segments(relative.path_.string()));
        has_query_ = relative.has_query_;
        query_ = relative.query_;
    } else if (relative.path_.string().empty()) {
        has_authority_ = base.has_authority_;
        has_authentication_ = base.has_authentication_;
        has_port_ = base.has_port_;
        username_ = base.username_;
        password_ = base.password_;
        host_ = base.host_;
        port_ = base.port_;
        path_ = base.path_;

        if (relative.has_query_) {
            has_query_ = true;
            query_ = relative.query_;
        } else {
            has_query_ = base.has_query_;
            query_ = base.query_;
        }
    } else {
        has_authority_ = base.has_authority_;
        has_authentication_ = base.has_authentication_;
        has_port_ = base.has_port_;
        username_ = base.username_;
        password_ = base.password_;
        host_ = base.host_;
        port_ = base.port_;

        const std::string rel_path = relative.path_.string();

        if (!rel_path.empty() && rel_path.front() == '/') {
            path_ = xlnt::path(remove_dot_segments(rel_path));
        } else {
            const std::string base_path = base.path_.string();
            const std::string merged = merge_paths(base_path, rel_path, base.has_authority_);
            path_ = xlnt::path(remove_dot_segments(merged));
        }

        has_query_ = relative.has_query_;
        query_ = relative.query_;
    }

    ipv6_ = has_authority_ ? (relative.has_authority_ ? relative.ipv6_ : base.ipv6_) : false;
    has_fragment_ = relative.has_fragment_;
    fragment_ = relative.fragment_;
}

uri::uri(const uri& base, const xlnt::path& relative)
{
    uri ref;
    ref.absolute_ = false;
    ref.has_authority_ = false;
    ref.path_ = relative;
    ref.ipv6_ = base.ipv6_;

    *this = uri(base, ref);
}

bool
uri::is_relative() const
{
    return !absolute_;
}

bool
uri::is_absolute() const
{
    return absolute_;
}

std::string
uri::scheme() const
{
    return scheme_;
}

std::string
uri::authority() const
{
    if (!has_authority_) {
        return std::string();
    }

    std::string result;

    if (has_authentication_) {
        result += username_;
        if (!password_.empty()) {
            result += ':';
            result += password_;
        }
        result += '@';
    }

    if (ipv6_) {
        result += '[' + host_ + ']';
    } else {
        result += host_;
    }

    if (has_port_) {
        result += ':';
        result += std::to_string(port_);
    }

    return result;
}

bool
uri::has_authentication() const
{
    return has_authentication_;
}

std::string
uri::authentication() const
{
    if (!has_authentication_) {
        return std::string();
    }

    if (password_.empty()) {
        return username_;
    }

    return username_ + ":" + password_;
}

std::string
uri::username() const
{
    return username_;
}

std::string
uri::password() const
{
    return password_;
}

std::string
uri::host() const
{
    return host_;
}

bool
uri::has_port() const
{
    return has_port_;
}

std::size_t
uri::port() const
{
    return port_;
}

const class path&
uri::path() const
{
    return path_;
}

bool
uri::has_query() const
{
    return has_query_;
}

std::string
uri::query() const
{
    return query_;
}

bool
uri::has_fragment() const
{
    return has_fragment_;
}

std::string
uri::fragment() const
{
    return fragment_;
}

std::string
uri::to_string() const
{
    std::string result;

    if (!scheme_.empty()) {
        result += scheme_;
        result += ':';
    }

    if (has_authority_) {
        result += "//";
        result += authority();
    }

    result += path_.string();

    if (has_query_) {
        result += '?';
        result += query_;
    }

    if (has_fragment_) {
        result += '#';
        result += fragment_;
    }

    return result;
}

uri
uri::make_absolute(const uri& base)
{
    if (is_absolute()) {
        return *this;
    }

    return uri(base, *this);
}

uri
uri::make_reference(const uri& base)
{
    if (!is_absolute()) {
        return *this;
    }

    if (lower_copy(scheme_) != lower_copy(base.scheme_) || has_authority_ != base.has_authority_
        || has_authentication_ != base.has_authentication_ || has_port_ != base.has_port_
        || lower_copy(host_) != lower_copy(base.host_) || username_ != base.username_
        || password_ != base.password_ || port_ != base.port_) {
        return *this;
    }

    uri result;
    result.absolute_ = false;
    result.has_authority_ = false;
    result.has_authentication_ = false;
    result.has_port_ = false;
    result.has_query_ = has_query_;
    result.query_ = query_;
    result.has_fragment_ = has_fragment_;
    result.fragment_ = fragment_;

    result.path_ = xlnt::path(
        relative_path_from_base(base.path_.string(), path_.string(), base.has_authority_));
    return result;
}

bool
uri::has_authority() const
{
    return has_authority_;
}

bool
uri::operator==(const uri& other) const
{
    return absolute_ == other.absolute_ && has_authority_ == other.has_authority_
        && has_authentication_ == other.has_authentication_ && has_port_ == other.has_port_
        && has_query_ == other.has_query_ && has_fragment_ == other.has_fragment_
        && lower_copy(scheme_) == lower_copy(other.scheme_) && username_ == other.username_
        && password_ == other.password_ && lower_copy(host_) == lower_copy(other.host_)
        && port_ == other.port_ && query_ == other.query_ && fragment_ == other.fragment_
        && normalize_for_compare(path_.string()) == normalize_for_compare(other.path_.string());
}

bool
uri::operator!=(const uri& other) const
{
    return !(*this == other);
}

} // namespace xlnt

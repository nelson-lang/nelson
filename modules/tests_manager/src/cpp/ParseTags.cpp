//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <sstream>
#include <fstream>
#include "ParseTags.hpp"
#include "i18n.hpp"
#include "IsFile.hpp"
#include "FileParts.hpp"
#include "ParseFile.hpp"
#include "Comments.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    static bool isEmptyLine(std::string line)
    {
        std::string str = boost::algorithm::trim_left_copy(line);
        return str == "";
    }
    //=============================================================================
    bool ParseTags(Evaluator *eval, std::wstring filename, TestTags &options, std::wstring &msg)
    {
        if (!IsFile(filename))
        {
            msg = _W("an existing file expected.");
            return false;
        }
        std::wstring basename = FilePartsFilename(filename);
        std::wstring ext = FilePartsExtension(filename);
        bool isValidFilename = (boost::algorithm::starts_with(basename, L"test_") ||
                                boost::algorithm::starts_with(basename, L"bench_") ||
                                boost::algorithm::starts_with(basename, L"bug_")) && (ext == L".nls");
        if (!isValidFilename)
        {
            if (ext != L".nls")
            {
                msg = _W("wrong file extension .nls expected.");
                return false;
            }
            else
            {
                msg = _W("wrong file prefix 'test_' or 'bug_' expected.");
                return false;
            }
        }
        std::ifstream istream;
#ifdef _MSC_VER
        istream.open(filename);
#else
        istream.open(wstring_to_utf8(filename));
#endif
        bool firstCliTag = true;
        bool firstAdvCliTag = true;
        bool firstGuiTag = true;
        bool firstNotFixedTag = true;
        bool firstInteractiveTag = true;
        bool firstCheckRefTag = true;
        bool firstEnglishTag = true;
        bool firstWindowsTag = true;
        bool firstMacTag = true;
        bool firstLinuxTag = true;
        bool firstWithDisplayTag = true;
        bool firstReleaseOnlyTag = true;
		bool firstExcelRequiredTag = true;

        if (istream.is_open())
        {
            std::string line;
            while (!istream.eof())
            {
                std::getline(istream, line);
                if (!isCommentedLine(line) && (!isEmptyLine(line)))
                {
                    istream.close();
                    break;
                }
                std::string removedLeftBlank = boost::algorithm::trim_left_copy(line);
                if (boost::algorithm::contains(removedLeftBlank, WITH_DISPLAY_TAG) && firstWithDisplayTag)
                {
                    if (options.isWithDisplay() && !firstWithDisplayTag)
                    {
                        msg = _W("duplicated tag detected: <--WITH DISPLAY TEST-->.");
                        istream.close();
                        return false;
                    }
                    options.setWithDisplay(true);
                    firstWithDisplayTag = false;
                }
                if (boost::algorithm::contains(removedLeftBlank, NOT_FIXED_TAG) && firstNotFixedTag)
                {
                    if (options.isNotFixed() && !firstNotFixedTag)
                    {
                        msg = _W("duplicated tag detected: <--INTERACTIVE TEST-->.");
                        istream.close();
                        return false;
                    }
                    options.setNotFixed(true);
                    firstNotFixedTag = false;
                }
                if (boost::algorithm::contains(removedLeftBlank, INTERACTIVE_TEST_TAG) && firstInteractiveTag)
                {
                    if (options.isInteractiveTest() && !firstInteractiveTag)
                    {
                        msg = _W("duplicated tag detected: <--INTERACTIVE TEST-->.");
                        istream.close();
                        return false;
                    }
                    options.setInteractiveTest(true);
                    firstInteractiveTag = false;
                }
                if (boost::algorithm::contains(removedLeftBlank, ENGLISH_IMPOSED_TAG) && firstEnglishTag)
                {
                    if (options.isEnglishImposed() && !firstEnglishTag)
                    {
                        msg = _W("duplicated tag detected: <--ENGLISH IMPOSED-->.");
                        istream.close();
                        return false;
                    }
                    options.setEnglishImposed(true);
                    firstEnglishTag = false;
                }
                if (boost::algorithm::contains(removedLeftBlank, CLI_MODE_TAG) && firstCliTag)
                {
                    if ((options.isAdvCliMode() && !firstAdvCliTag)|| (options.isGuiMode() && !firstGuiTag))
                    {
                        msg = _W("Check tags used.");
                        istream.close();
                        return false;
                    }
                    if (options.isCliMode() && !firstCliTag)
                    {
                        msg = _W("duplicated tag detected: <--CLI MODE-->.");
                        istream.close();
                        return false;
                    }
                    options.setCliMode(true);
                    options.setAdvCliMode(false);
                    options.setGuiMode(false);
                    firstCliTag = false;
                }
                if (boost::algorithm::contains(removedLeftBlank, ADV_CLI_MODE_TAG) && firstAdvCliTag)
                {
                    if ((options.isCliMode() && !firstCliTag) || (options.isGuiMode() && !firstGuiTag))
                    {
                        msg = _W("Check tags used.");
                        istream.close();
                        return false;
                    }
                    if (options.isAdvCliMode() && firstAdvCliTag)
                    {
                        msg = _W("duplicated tag detected: <--ADV-CLI MODE-->.");
                        istream.close();
                        return false;
                    }
                    options.setAdvCliMode(true);
                    options.setCliMode(false);
                    options.setGuiMode(false);
                    firstAdvCliTag = false;
                }
                if (boost::algorithm::contains(removedLeftBlank, GUI_MODE_TAG) && firstGuiTag)
                {
                    if ((options.isCliMode() && !firstCliTag) || (options.isAdvCliMode() && !firstAdvCliTag))
                    {
                        msg = _W("Multiple exclusive tags detected: <--GUI MODE-->.");
                        istream.close();
                        return false;
                    }
                    if (options.isGuiMode() && firstGuiTag)
                    {
                        msg = _W("duplicated tag detected: <--GUI MODE-->.");
                        istream.close();
                        return false;
                    }
                    options.setGuiMode(true);
                    options.setAdvCliMode(false);
                    options.setCliMode(false);
                    firstGuiTag = false;
                }
                if (boost::algorithm::contains(removedLeftBlank, CHECK_REF_TAG) && firstCheckRefTag)
                {
                    if (options.isCheckRef() && !firstAdvCliTag)
                    {
                        msg = _W("duplicated tag detected: <--CHECK REF-->.");
                        istream.close();
                        return false;
                    }
                    options.setCheckRef(true);
                    firstCheckRefTag = false;
                }
                if (boost::algorithm::contains(removedLeftBlank, WINDOWS_ONLY_TAG) && firstWindowsTag)
                {
                    if ((options.isUnixOnly() && !firstLinuxTag) || (options.isMacOnly() && !firstMacTag))
                    {
                        msg = _W("Multiple exclusive tags detected: <--WINDOWS ONLY-->.");
                        istream.close();
                        return false;
                    }
                    if (options.isWindowsOnly() && !firstWindowsTag)
                    {
                        msg = _W("duplicated tag detected: <--WINDOWS ONLY-->.");
                        istream.close();
                        return false;
                    }
                    options.setWindowsOnly(true);
                    firstWindowsTag = false;
                }
                if (boost::algorithm::contains(removedLeftBlank, MACOS_ONLY_TAG) && firstMacTag)
                {
                    if ((options.isUnixOnly() && !firstLinuxTag) || (options.isWindowsOnly() && !firstWindowsTag))
                    {
                        msg = _W("Multiple exclusive tags detected: <--MACOS ONLY-->.");
                        istream.close();
                        return false;
                    }
                    if (options.isMacOnly() && !firstMacTag)
                    {
                        msg = _W("duplicated tag detected: <--MACOS ONLY-->.");
                        istream.close();
                        return false;
                    }
                    options.setMacOnly(true);
                    firstMacTag = false;
                }
                if (boost::algorithm::contains(removedLeftBlank, UNIX_ONLY_TAG) && firstLinuxTag)
                {
                    if ((options.isMacOnly() && !firstMacTag) || (options.isWindowsOnly() && !firstWindowsTag))
                    {
                        msg = _W("Multiple exclusive tags detected: <--UNIX ONLY-->.");
                        istream.close();
                        return false;
                    }
                    if (options.isUnixOnly() && !firstLinuxTag)
                    {
                        msg = _W("duplicated tag detected: <--UNIX ONLY-->.");
                        istream.close();
                        return false;
                    }
                    options.setUnixOnly(true);
                    firstLinuxTag = false;
                }
                if (boost::algorithm::contains(removedLeftBlank, RELEASE_ONLY_TAG) && firstReleaseOnlyTag)
                {
                    if (!firstReleaseOnlyTag)
                    {
                        msg = _W("duplicated tag detected: <--RELEASE ONLY-->.");
                        istream.close();
                        return false;
                    }
                    options.setReleaseOnly(true);
                    firstReleaseOnlyTag = false;
                }
				if (boost::algorithm::contains(removedLeftBlank, EXCEL_REQUIRED_TAG) && firstExcelRequiredTag)
				{
					if (!firstExcelRequiredTag)
					{
						msg = _W("duplicated tag detected: <--EXCEL REQUIRED-->.");
						istream.close();
						return false;
					}
					options.setExcelRequired(true);
					firstExcelRequiredTag = false;
				}
			}
            istream.close();
            if (options.isGuiMode() || options.isAdvCliMode())
            {
                options.setWithDisplay(true);
            }
        }
        return true;
    }
    //=============================================================================
}
//=============================================================================

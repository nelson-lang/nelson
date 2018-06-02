//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include "HtmlToPdf.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include <QtCore/QFileInfo>
#include <QtGui/QTextDocument>
#include <QtPrintSupport/QPrinter>
#include <fstream>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::ifstream&
safegetline(std::ifstream& os, std::string& line)
{
    std::string myline;
    if (getline(os, myline)) {
        if (myline.size() && myline[myline.size() - 1] == '\r') {
            line = myline.substr(0, myline.size() - 1);
        } else {
            line = myline;
        }
    }
    return os;
}
//=============================================================================
bool
HtmlFileToPdfFile(const std::wstring& htmlsrcfilename, const std::wstring& pdfdestfilename)
{
    std::ifstream istream;
#ifdef _MSC_VER
    istream.open(htmlsrcfilename);
#else
    istream.open(wstring_to_utf8(htmlsrcfilename));
#endif
    std::wstring lines;
    if (istream.is_open()) {
        while (!istream.eof()) {
            std::string buffer;
            safegetline(istream, buffer);
            lines = lines + utf8_to_wstring(buffer);
        }
        istream.close();
    } else {
        return false;
    }
    if (!lines.empty()) {
        return HtmlStreamToPdfFile(lines, pdfdestfilename);
    }
    return true;
}
//=============================================================================
bool
HtmlStreamToPdfFile(const std::wstring& htmlstream, const std::wstring& pdfdestfilename)
{
    QPrinter printer(QPrinter::PrinterResolution);
    printer.setOutputFormat(QPrinter::PdfFormat);
    printer.setPaperSize(QPrinter::A4);
    QString dest = wstringToQString(pdfdestfilename);
    if (QFileInfo(dest).suffix().isEmpty()) {
        dest.append(".pdf");
    }
    printer.setOutputFileName(wstringToQString(pdfdestfilename));
    QTextDocument doc;
    doc.setHtml(wstringToQString(htmlstream));
    doc.setPageSize(printer.pageRect().size());
    doc.print(&printer);
    return true;
}
//=============================================================================
}
//=============================================================================

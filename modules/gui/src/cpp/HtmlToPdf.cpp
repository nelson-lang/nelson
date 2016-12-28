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
#include <fstream>
#include <QtGui/QTextDocument>
#include <QtPrintSupport/QPrinter>
#include <QtCore/QFileInfo>
#include "QStringConverter.hpp"
#include "HtmlToPdf.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    bool HtmlFileToPdfFile(std::wstring htmlsrcfilename, std::wstring pdfdestfilename)
    {
        std::ifstream istream;
#ifdef _MSC_VER
        istream.open(htmlsrcfilename);
#else
        istream.open(wstring_to_utf8(htmlsrcfilename));
#endif
        std::wstring lines;
        if (istream.is_open())
        {
            while (!istream.eof())
            {
                std::string buffer;
                std::getline(istream, buffer);
                lines = lines + utf8_to_wstring(buffer);
            }
            istream.close();
        }
        else
        {
            return false;
        }
        if (!lines.empty())
        {
            return HtmlStreamToPdfFile(lines, pdfdestfilename);
        }
        return true;
    }
    //=============================================================================
    bool HtmlStreamToPdfFile(std::wstring htmlstream, std::wstring pdfdestfilename)
    {
        QPrinter printer(QPrinter::PrinterResolution);
        printer.setOutputFormat(QPrinter::PdfFormat);
        printer.setPaperSize(QPrinter::A4);
        QString dest = wstringToQString(pdfdestfilename);
        if (QFileInfo(dest).suffix().isEmpty())
        {
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

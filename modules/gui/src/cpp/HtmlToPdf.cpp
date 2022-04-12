//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HtmlToPdf.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include <QtCore/QFileInfo>
#include <QtGui/QTextDocument>
#include <QtPrintSupport/QPrinter>
#if QT_VERSION >= QT_VERSION_CHECK(5, 3, 0)
#include <QtGui/QPageSize>
#endif
#include <fstream>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::ifstream&
safegetline(std::ifstream& os, std::string& line)
{
    std::string myline;
    if (getline(os, myline)) {
        if (!myline.empty() && myline[myline.size() - 1] == '\r') {
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
#if QT_VERSION >= QT_VERSION_CHECK(5, 3, 0)
    printer.setPageSize(QPageSize(QPageSize::A4));
#else
    printer.setPaperSize(QPrinter::A4);
#endif
    QString dest = wstringToQString(pdfdestfilename);
    if (QFileInfo(dest).suffix().isEmpty()) {
        dest.append(".pdf");
    }
    printer.setOutputFileName(wstringToQString(pdfdestfilename));
    QTextDocument doc;
    doc.setHtml(wstringToQString(htmlstream));
#if QT_VERSION >= QT_VERSION_CHECK(5, 3, 0)
    doc.setPageSize(printer.pageLayout().fullRect().size());
#else
    doc.setPageSize(printer.pageRect().size());
#endif
    doc.print(&printer);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================

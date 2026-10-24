//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CurrentDirectoryWidget.h"
#include "DesktopCommandPalette.h"
#include "DesktopPanelRail.h"
#include "DesktopUiStyle.h"
#include "QtDockTitleBar.h"
#include "QtTranslation.hpp"
#include "QtWaitCursor.hpp"
#include "NelsonApplication.hpp"
#include "RecentDirectoriesStore.hpp"
#include "DialogDirectoriesStore.hpp"
#include <QtCore/QDir>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QTemporaryDir>
#include <QtCore/QUrl>
#include <QtCore/QtGlobal>
#include <QtGui/QAction>
#include <QtGui/QActionGroup>
#include <QtGui/QImage>
#include <QtGui/QCursor>
#include <QtGui/QKeyEvent>
#include <QtGui/QFileOpenEvent>
#include <QtWidgets/QApplication>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QListWidget>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenu>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QToolButton>
#include <algorithm>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
//=============================================================================
namespace Nelson {
QString
TR(const std::string& text)
{
    return QString::fromStdString(text);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
static void
check(bool condition, const char* message)
{
    if (!condition) {
        std::cerr << "FAILED: " << message << std::endl;
        std::exit(EXIT_FAILURE);
    }
}
//=============================================================================
static void
testFileOpenEvents(NelsonApplication& application)
{
    QTemporaryDir directory;
    check(directory.isValid(), "the file-open test directory must be available");
    const QString filename = directory.filePath("example.nflow");
    QFile file(filename);
    check(file.open(QIODevice::WriteOnly), "the file-open test file must be created");
    file.close();

    QFileOpenEvent event(filename);
    QApplication::sendEvent(&application, &event);
    check(application.pendingFileCount() == 1,
        "file-open events received before initialization must be queued");

    std::vector<std::wstring> openedFilenames;
    application.setFileOpenHandler([&openedFilenames](const std::wstring& path) {
        openedFilenames.push_back(path);
        return true;
    });
    check(application.pendingFileCount() == 0,
        "queued file-open events must be dispatched after initialization");
    check(openedFilenames.front() == QFileInfo(filename).absoluteFilePath().toStdWString(),
        "the file-open handler must receive an absolute path");

    QFileOpenEvent urlEvent(QUrl::fromLocalFile(filename));
    QApplication::sendEvent(&application, &urlEvent);
    check(openedFilenames.size() == 2,
        "local file URLs must be dispatched through the file-open handler");
}
//=============================================================================
static void
testExamplesGalleryIntegration()
{
    const std::filesystem::path source = std::filesystem::path(NELSON_TEST_REPOSITORY_DIR)
        / "modules" / "gui" / "src" / "cpp" / "QtMainWindow.cpp";
    std::ifstream stream(source);
    const std::string text((std::istreambuf_iterator<char>(stream)), {});
    check(text.find("postCommand(L\"demo()\")") != std::string::npos,
        "the examples action must call demo()");
    check(text.find("helpMenu->addAction(examplesAction)") != std::string::npos,
        "the Help menu must expose examples");
    check(text.find("helpActions.prepend(examplesAction)") != std::string::npos,
        "the command palette must expose examples");
    check(text.find("panelsSidebar->addCommandAction(examplesAction)") != std::string::npos,
        "the panel rail must expose examples");
}
//=============================================================================
static void
testThemeTokens()
{
    QPalette light;
    light.setColor(QPalette::Window, Qt::white);
    light.setColor(QPalette::Highlight, QColor("#126bc5"));
    QPalette dark;
    dark.setColor(QPalette::Window, Qt::black);
    dark.setColor(QPalette::Highlight, QColor("#74b5ff"));

    const DesktopUiColors lightColors = desktopUiColors(light);
    const DesktopUiColors darkColors = desktopUiColors(dark);
    check(lightColors.window == QColor("#ffffff") && lightColors.panel == QColor("#ffffff")
            && lightColors.title == QColor("#f3f3f3") && lightColors.border == QColor("#d4d4d4")
            && lightColors.sidebar == QColor("#e8e8e8") && lightColors.accent == QColor("#0072bd"),
        "the light shell must use the common desktop palette");
    check(darkColors.window == QColor("#1e1e1e") && darkColors.panel == QColor("#252526")
            && darkColors.title == QColor("#252526") && darkColors.border == QColor("#333333")
            && darkColors.sidebar == QColor("#333333") && darkColors.accent == QColor("#007acc"),
        "the dark shell must use the common desktop palette");
    const QString lightStyle = desktopShellStyleSheet(light);
    check(lightStyle.contains(lightColors.accent.name().mid(1)),
        "the stylesheet must contain the accent token");
#ifdef Q_OS_MACOS
    check(!lightStyle.contains("QMenuBar#NelsonMenuBar"),
        "the native macOS menu bar must not reserve space in the main window");
#else
    check(lightStyle.contains("QMenuBar#NelsonMenuBar { min-height: 32px"),
        "the in-window menu bar must use the common desktop metric");
#endif
    check(lightStyle.contains("min-height: 38px") && lightStyle.contains("min-height: 24px")
            && lightStyle.contains("min-width: 40px"),
        "the shell stylesheet must use the common desktop metrics");
}
//=============================================================================
static void
testSvgTint()
{
    QPalette palette;
    const QColor tint("#d21f73");
    const QColor disabledTint("#65717a");
    const QColor paletteHighlight("#1c73d1");
    const QColor activeTint("#0072bd");
    const QColor selectedTint("#ffffff");
    palette.setColor(QPalette::Window, Qt::white);
    palette.setColor(QPalette::ButtonText, tint);
    palette.setColor(QPalette::Disabled, QPalette::ButtonText, disabledTint);
    palette.setColor(QPalette::Highlight, paletteHighlight);
    palette.setColor(QPalette::HighlightedText, selectedTint);
    const QString iconPath = QString(NELSON_TEST_REPOSITORY_DIR) + "/resources/action-run.svg";
    const QIcon icon = desktopTintedIcon(iconPath, palette);
    const QImage image = icon.pixmap(QSize(24, 24), QIcon::Normal)
                             .toImage()
                             .convertToFormat(QImage::Format_ARGB32);
    const QImage disabledImage = icon.pixmap(QSize(24, 24), QIcon::Disabled)
                                     .toImage()
                                     .convertToFormat(QImage::Format_ARGB32);
    const QImage activeImage = icon.pixmap(QSize(24, 24), QIcon::Active)
                                   .toImage()
                                   .convertToFormat(QImage::Format_ARGB32);
    const QImage selectedImage = icon.pixmap(QSize(24, 24), QIcon::Selected)
                                     .toImage()
                                     .convertToFormat(QImage::Format_ARGB32);
    bool foundTintedPixel = false;
    bool foundDisabledPixel = false;
    bool foundActivePixel = false;
    bool foundSelectedPixel = false;
    for (int y = 0; y < image.height() && !foundTintedPixel; ++y) {
        for (int x = 0; x < image.width(); ++x) {
            const QColor pixel = image.pixelColor(x, y);
            if (pixel.alpha() > 32 && pixel.red() == tint.red() && pixel.green() == tint.green()
                && pixel.blue() == tint.blue()) {
                foundTintedPixel = true;
                break;
            }
        }
    }
    for (int y = 0; y < disabledImage.height() && !foundDisabledPixel; ++y) {
        for (int x = 0; x < disabledImage.width(); ++x) {
            const QColor pixel = disabledImage.pixelColor(x, y);
            if (pixel.alpha() > 32 && pixel.red() == disabledTint.red()
                && pixel.green() == disabledTint.green() && pixel.blue() == disabledTint.blue()) {
                foundDisabledPixel = true;
                break;
            }
        }
    }
    for (int y = 0; y < activeImage.height() && !foundActivePixel; ++y) {
        for (int x = 0; x < activeImage.width(); ++x) {
            const QColor pixel = activeImage.pixelColor(x, y);
            if (pixel.alpha() > 32 && pixel.red() == activeTint.red()
                && pixel.green() == activeTint.green() && pixel.blue() == activeTint.blue()) {
                foundActivePixel = true;
                break;
            }
        }
    }
    for (int y = 0; y < selectedImage.height() && !foundSelectedPixel; ++y) {
        for (int x = 0; x < selectedImage.width(); ++x) {
            const QColor pixel = selectedImage.pixelColor(x, y);
            if (pixel.alpha() > 32 && pixel.red() == selectedTint.red()
                && pixel.green() == selectedTint.green() && pixel.blue() == selectedTint.blue()) {
                foundSelectedPixel = true;
                break;
            }
        }
    }
    check(foundTintedPixel, "SVG pixels must be tinted from QPalette");
    check(foundDisabledPixel, "disabled SVG pixels must be tinted from QPalette");
    check(foundActivePixel, "active SVG pixels must use the common desktop accent");
    check(foundSelectedPixel, "selected SVG pixels must use the selected text color");
}
//=============================================================================
static bool
hasVisiblePixel(const QImage& image)
{
    for (int y = 0; y < image.height(); ++y) {
        for (int x = 0; x < image.width(); ++x) {
            if (image.pixelColor(x, y).alpha() > 32) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
static void
testNelsonShellSvgSet()
{
    QPalette palette;
    palette.setColor(QPalette::ButtonText, QColor("#224466"));
    const QString resources = QString(NELSON_TEST_REPOSITORY_DIR) + "/resources/";
    const QStringList iconFiles = { "action-editor.svg", "action-nflow.svg", "action-run.svg",
        "action-stop.svg", "panel-command.svg", "panel-files.svg", "panel-history.svg",
        "panel-profiler.svg", "panel-terminal.svg", "panel-variables.svg", "panel-workspace.svg" };
    for (const QString& iconFile : iconFiles) {
        const QString iconPath = resources + iconFile;
        check(QFileInfo::exists(iconPath), "Nelson shell SVG files must exist");
        const QIcon icon = desktopTintedIcon(iconPath, palette);
        check(!icon.isNull(), "Nelson shell SVG files must load as icons");
        const QImage image = icon.pixmap(QSize(24, 24), QIcon::Normal)
                                 .toImage()
                                 .convertToFormat(QImage::Format_ARGB32);
        check(!image.isNull() && hasVisiblePixel(image),
            "Nelson shell SVG files must rasterize to visible pixels");
    }
}
//=============================================================================
static bool
containsRatio(const QList<qreal>& ratios, qreal expected)
{
    for (qreal ratio : ratios) {
        if (qAbs(ratio - expected) < 0.001) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static void
testHighDpiIconVariants()
{
    const QList<qreal> ratios = desktopIconDevicePixelRatios();
    check(containsRatio(ratios, 1.0), "SVG icons must include a 1x rasterization");
    check(containsRatio(ratios, 1.25), "SVG icons must include a 125 percent rasterization");
    check(containsRatio(ratios, 1.5), "SVG icons must include a 150 percent rasterization");
    check(containsRatio(ratios, 2.0), "SVG icons must include a 200 percent rasterization");
    check(containsRatio(ratios, 3.0), "SVG icons must include a 300 percent rasterization");
}
//=============================================================================
static void
testAccessibleActionText()
{
    QAction action("&Files");
    QAction escapedAction("Save && close");
    check(desktopAccessibleActionText(&action) == "Files",
        "accessible action text must remove keyboard mnemonics");
    check(desktopAccessibleActionText(&escapedAction) == "Save & close",
        "accessible action text must preserve escaped ampersands");
}
//=============================================================================
static void
testDesktopCommandShortcuts()
{
    const QString shortcut
        = desktopCommandShortcut(Qt::Key_P, true).toString(QKeySequence::PortableText);
#ifdef Q_OS_MACOS
    check(shortcut.contains("Meta") && shortcut.contains("Shift") && shortcut.endsWith("+P"),
        "desktop command shortcuts must use Command on macOS");
#else
    check(shortcut.contains("Ctrl") && shortcut.contains("Shift") && shortcut.endsWith("+P"),
        "desktop command shortcuts must use Control on Windows and Linux");
#endif
}
//=============================================================================
static void
testExclusiveActionSelectionCanBeCleared()
{
    QActionGroup group(nullptr);
    group.setExclusive(true);
    QAction first("Two columns", &group);
    QAction second("Three columns", &group);
    QAction third("Command Window only", &group);
    const QList<QAction*> actions = { &first, &second, &third };
    for (QAction* action : actions) {
        action->setCheckable(true);
        group.addAction(action);
    }
    first.setChecked(true);
    desktopClearExclusiveActionSelection(&group, actions);
    check(group.isExclusive(), "clearing layout presets must preserve action group exclusivity");
    check(!first.isChecked() && !second.isChecked() && !third.isChecked(),
        "clearing layout presets must uncheck every preset even in an exclusive action group");
    second.setChecked(true);
    check(second.isChecked() && !first.isChecked() && !third.isChecked(),
        "layout preset actions must remain exclusive after clearing");
}
//=============================================================================
static void
testPaletteRanking()
{
    const int prefix = DesktopCommandPalette::matchScore("Open workspace", "open");
    const int word = DesktopCommandPalette::matchScore("Panel open workspace", "open");
    const int contains = DesktopCommandPalette::matchScore("Reopen workspace", "open");
    check(prefix > word && word > contains, "prefix matches must rank first");
    check(DesktopCommandPalette::matchScore("Open workspace", "OPEN") == prefix,
        "matching must be case insensitive");
    check(DesktopCommandPalette::matchScore("Open workspace", "missing") < 0,
        "unmatched entries must be excluded");

    DesktopCommandPalette palette;
    QAction openWorkspace("Open workspace", &palette);
    QAction panelOpenWorkspace("Panel open workspace", &palette);
    QAction reopenWorkspace("Reopen workspace", &palette);
    QAction escapedAction("Save && close", &palette);
    QAction disabledAction("Disabled command", &palette);
    disabledAction.setEnabled(false);
    bool triggered = false;
    bool disabledTriggered = false;
    QObject::connect(&openWorkspace, &QAction::triggered, [&]() { triggered = true; });
    QObject::connect(&disabledAction, &QAction::triggered, [&]() { disabledTriggered = true; });
    palette.addAction(&escapedAction, "File");
    palette.addAction(&reopenWorkspace, "File");
    palette.addAction(&panelOpenWorkspace, "Panel");
    palette.addAction(&openWorkspace, "Workspace");
    palette.addAction(&disabledAction, "File");
    palette.openWithQuery("open");
    QApplication::processEvents();

    QListWidget* results = palette.findChild<QListWidget*>();
    QLineEdit* search = palette.findChild<QLineEdit*>();
    check(results && results->currentItem(), "the command palette must select a best match");
    check(search != nullptr, "the command palette must expose its search field");
    check(results->currentItem()->text().startsWith("Open workspace"),
        "prefix matches must appear first in the command palette UI");
    const bool invoked
        = QMetaObject::invokeMethod(&palette, "triggerCurrent", Qt::DirectConnection);
    check(invoked, "the command palette trigger slot must be invokable");
    check(triggered, "triggering the selected command palette row must fire its action");

    palette.openWithQuery("save");
    QApplication::processEvents();
    check(results->currentItem() && results->currentItem()->text().startsWith("Save & close"),
        "the command palette must preserve escaped ampersands in action labels");

    palette.openWithQuery("disabled");
    QApplication::processEvents();
    check(results->currentItem() && results->currentItem()->text().startsWith("Disabled command"),
        "disabled command palette actions must still be searchable");
    const bool disabledInvoke
        = QMetaObject::invokeMethod(&palette, "triggerCurrent", Qt::DirectConnection);
    check(disabledInvoke, "the command palette trigger slot must handle disabled results");
    check(!disabledTriggered, "disabled command palette actions must not trigger");
    check(palette.isVisible(), "triggering a disabled command must keep the palette open");

    palette.removeAction(&escapedAction);
    palette.openWithQuery("save");
    QApplication::processEvents();
    check(!results->currentItem(), "removed command palette actions must no longer appear");

    QAction* deletedAction = new QAction("Deleted action", &palette);
    bool deletedActionTriggered = false;
    QObject::connect(deletedAction, &QAction::triggered, [&]() { deletedActionTriggered = true; });
    palette.addAction(deletedAction, "File");
    palette.openWithQuery("deleted");
    QApplication::processEvents();
    check(results->currentItem() && results->currentItem()->text().startsWith("Deleted action"),
        "the command palette test must start with a visible soon-deleted action");
    delete deletedAction;
    const bool staleInvoke
        = QMetaObject::invokeMethod(&palette, "triggerCurrent", Qt::DirectConnection);
    check(staleInvoke, "the command palette trigger slot must still be invokable after deletion");
    check(!deletedActionTriggered,
        "the command palette must not trigger an action deleted after result creation");
    check(!results->currentItem(),
        "deleted command palette actions must be removed when a stale result is triggered");

    palette.openWithQuery("workspace");
    QApplication::processEvents();
    search->setFocus();
    const int resultCount = results->count();
    QKeyEvent downEvent(QEvent::KeyPress, Qt::Key_Down, Qt::NoModifier);
    QApplication::sendEvent(&palette, &downEvent);
    check(results->hasFocus() && resultCount > 0 && results->currentRow() == 0,
        "pressing Down in the command search field must focus the first result");
    search->setFocus();
    QKeyEvent upEvent(QEvent::KeyPress, Qt::Key_Up, Qt::NoModifier);
    QApplication::sendEvent(&palette, &upEvent);
    check(results->hasFocus() && resultCount > 0 && results->currentRow() == resultCount - 1,
        "pressing Up in the command search field must focus the last result");
    QKeyEvent escapeEvent(QEvent::KeyPress, Qt::Key_Escape, Qt::NoModifier);
    QApplication::sendEvent(&palette, &escapeEvent);
    QApplication::processEvents();
    check(!palette.isVisible(), "pressing Escape must close the command palette");
}
//=============================================================================
static void
pressReturn(QLineEdit* lineEdit)
{
    CurrentDirectoryWidget* widget = nullptr;
    QWidget* parent = lineEdit;
    while (parent && !widget) {
        widget = qobject_cast<CurrentDirectoryWidget*>(parent);
        parent = parent->parentWidget();
    }
    const bool invoked
        = widget && QMetaObject::invokeMethod(widget, "acceptDirectoryText", Qt::DirectConnection);
    check(invoked, "the directory field must validate its text through the Enter handler");
}
//=============================================================================
static void
testCurrentDirectoryWidget()
{
    check(CurrentDirectoryWidget::isValidDirectory(QString(NELSON_TEST_REPOSITORY_DIR)),
        "the repository directory must be valid");
    check(!CurrentDirectoryWidget::isValidDirectory(QString()),
        "an empty directory must be rejected");
    check(!CurrentDirectoryWidget::isValidDirectory(
              QString(NELSON_TEST_REPOSITORY_DIR) + "/directory-that-does-not-exist"),
        "a missing directory must be rejected");

    const QString previousDirectory = QDir::currentPath();
    QDir::setCurrent(QString(NELSON_TEST_REPOSITORY_DIR));

    CurrentDirectoryWidget widget;
    widget.show();
    QApplication::processEvents();
    QLineEdit* field = widget.findChild<QLineEdit*>("NelsonDirectoryField");
    check(field != nullptr, "the directory widget must expose its line edit by object name");
    QToolButton* browseButton = widget.findChild<QToolButton*>("NelsonDirectoryBrowseButton");
    check(browseButton != nullptr,
        "the directory widget must expose its browse button by object name");
    check(browseButton->minimumSize().width() >= 32 && browseButton->minimumSize().height() >= 32,
        "the directory browse button must provide at least a 32 px hit target");
    QComboBox* combo = widget.findChild<QComboBox*>("NelsonDirectoryCombo");
    check(combo != nullptr && combo->isEditable(),
        "the directory widget must expose an editable recent-directory combo box");

    QString requestedDirectory;
    bool browseRequested = false;
    QObject::connect(&widget, &CurrentDirectoryWidget::directoryChangeRequested,
        [&](const QString& directory) { requestedDirectory = directory; });
    QObject::connect(
        &widget, &CurrentDirectoryWidget::browseRequested, [&]() { browseRequested = true; });
    browseButton->click();
    check(browseRequested, "clicking the directory browse button must request browsing");

    const QString repositoryDirectory = QDir::cleanPath(QString(NELSON_TEST_REPOSITORY_DIR));
    field->setText(repositoryDirectory);
    pressReturn(field);
    check(requestedDirectory == repositoryDirectory,
        "pressing Enter on a valid directory must request a directory change");
    check(field->text() == QDir::toNativeSeparators(repositoryDirectory),
        "a valid directory must be displayed with native separators after Enter");

    const QString tempDirectory = QDir::cleanPath(repositoryDirectory + "/temp");
    widget.setDirectories({ repositoryDirectory, tempDirectory });
    check(combo->count() == 2 && combo->itemData(0).toString() == repositoryDirectory,
        "the directory combo box must preserve recent-directory order and path data");
    requestedDirectory.clear();
    const bool activated = QMetaObject::invokeMethod(
        &widget, "activateDirectory", Qt::DirectConnection, Q_ARG(int, 1));
    check(activated && requestedDirectory == tempDirectory,
        "activating a recent directory must request that directory");

    requestedDirectory.clear();
    field->setText(QString(NELSON_TEST_REPOSITORY_DIR) + "/directory-that-does-not-exist");
    pressReturn(field);
    check(requestedDirectory.isEmpty(),
        "pressing Enter on a missing directory must not request a directory change");
    check(field->text() == QDir::toNativeSeparators(QDir::currentPath()),
        "an invalid directory must restore the current path text");

    QDir::setCurrent(previousDirectory);
}
//=============================================================================
static void
testRecentDirectoriesStore()
{
    namespace fs = std::filesystem;
    QTemporaryDir temporaryDirectory;
    check(temporaryDirectory.isValid(), "the recent-directory temporary directory must exist");
    const fs::path root = fs::u8path(temporaryDirectory.path().toStdString());
    std::error_code ec;

    const fs::path storageFile = root / "recent_directories.json";
    Nelson::RecentDirectoriesStore store(storageFile);
    std::vector<fs::path> projects;
    for (int index = 0; index < 12; ++index) {
        fs::path project = root / ("project-" + std::to_string(index));
        fs::create_directories(project, ec);
        projects.emplace_back(project);
        check(store.add(project.generic_string()), "an existing directory must enter the MRU");
    }

    std::vector<std::string> directories = store.directories();
    check(directories.size() == Nelson::RecentDirectoriesStore::MAX_ENTRIES,
        "the recent-directory store must keep ten entries");
    check(fs::equivalent(fs::u8path(directories.front()), projects.back()),
        "the newest directory must be first");
    check(fs::equivalent(fs::u8path(directories.back()), projects[2]),
        "the oldest retained directory must be last");

    check(store.add(projects[5].generic_string()), "an existing MRU entry must be accepted");
    directories = Nelson::RecentDirectoriesStore(storageFile).directories();
    check(directories.size() == Nelson::RecentDirectoriesStore::MAX_ENTRIES
            && fs::equivalent(fs::u8path(directories.front()), projects[5]),
        "adding a duplicate must move it to the front without growing the list");
    check(!store.add((root / "missing").generic_string()),
        "a missing directory must not enter the MRU");

    fs::remove_all(projects[4], ec);
    directories = store.directories();
    const bool containsMissing
        = std::any_of(directories.begin(), directories.end(), [&](const std::string& directory) {
              return fs::path(directory).filename() == projects[4].filename();
          });
    check(!containsMissing, "missing persisted directories must be pruned");

    std::ofstream malformed(storageFile, std::ios::trunc);
    malformed << "{";
    malformed.close();
    check(store.directories().empty(), "malformed recent-directory JSON must be ignored");
}
//=============================================================================
static void
testDialogDirectoriesStore()
{
    namespace fs = std::filesystem;
    QTemporaryDir temporaryDirectory;
    check(temporaryDirectory.isValid(), "the dialog-directory temporary directory must exist");
    const fs::path root = fs::u8path(temporaryDirectory.path().toStdString());
    const fs::path openDirectory = root / "open";
    const fs::path saveDirectory = root / "save";
    const fs::path fallbackDirectory = root / "fallback";
    std::error_code ec;
    fs::create_directories(openDirectory, ec);
    fs::create_directories(saveDirectory, ec);
    fs::create_directories(fallbackDirectory, ec);
    check(!ec, "the dialog-directory fixtures must be created");

    const fs::path storageFile = root / "dialog_directories.json";
    Nelson::DialogDirectoriesStore store(storageFile);
    check(store.get("desktop.open").empty(), "a new dialog history must be empty");
    check(store.set("desktop.open", openDirectory.generic_string()),
        "an existing open directory must be persisted");
    check(store.set("desktop.save", saveDirectory.generic_string()),
        "an existing save directory must be persisted independently");
    check(fs::equivalent(fs::u8path(store.get("desktop.open")), openDirectory),
        "the open key must preserve its own directory");
    check(fs::equivalent(fs::u8path(store.get("desktop.save")), saveDirectory),
        "the save key must preserve its own directory");

    check(fs::equivalent(
              fs::u8path(store.resolve("desktop.open", ".", fallbackDirectory.generic_string())),
              openDirectory),
        "a placeholder path must resolve to the persisted directory");
    check(fs::equivalent(fs::u8path(store.resolve("desktop.open",
                             fallbackDirectory.generic_string(), openDirectory.generic_string())),
              fallbackDirectory),
        "an explicit valid directory must have priority");
    check(!store.set("desktop.open", (root / "missing").generic_string()),
        "a missing directory must not replace dialog history");

    fs::remove_all(openDirectory, ec);
    check(store.get("desktop.open").empty(), "a missing persisted dialog directory must be pruned");
    std::ofstream malformed(storageFile, std::ios::trunc);
    malformed << "{";
    malformed.close();
    check(store.get("desktop.save").empty(), "malformed dialog-directory JSON must be ignored");

    const fs::path blockedParent = root / "not-a-directory";
    std::ofstream blocker(blockedParent);
    blocker << "blocked";
    blocker.close();
    Nelson::DialogDirectoriesStore unwritable(blockedParent / "dialog_directories.json");
    check(!unwritable.set("desktop.open", fallbackDirectory.generic_string()),
        "a persistence error must be reported without throwing");
}
//=============================================================================
static void
testApplicationWaitCursorPolicy()
{
    check(QApplication::overrideCursor() == nullptr,
        "desktop shell tests must start without an application cursor override");
    Nelson::setApplicationWaitCursor();
#ifdef Q_OS_MACOS
    check(!Nelson::usesApplicationWaitCursor() && QApplication::overrideCursor() == nullptr,
        "macOS must avoid Qt's image-backed application wait cursor");
#else
    check(Nelson::usesApplicationWaitCursor() && QApplication::overrideCursor() != nullptr
            && QApplication::overrideCursor()->shape() == Qt::WaitCursor,
        "supported platforms must install the application wait cursor");
#endif
    Nelson::restoreApplicationWaitCursor();
    check(QApplication::overrideCursor() == nullptr,
        "restoring the application wait cursor must leave no override");
}
//=============================================================================
static void
testPanelRailTransitions(QApplication& application)
{
    DesktopPanelRail rail;
    QAction source("&Files", &rail);
    source.setCheckable(true);
    QAction secondSource("Workspace", &rail);
    secondSource.setCheckable(true);
    QAction command("Terminal", &rail);
    rail.addPanelAction(&source);
    rail.addPanelAction(&secondSource);
    rail.addRailSeparator();
    rail.addCommandAction(&command);
    rail.complete();

    QDockWidget dock;
    QDockWidget secondDock;
    rail.bindDock(&source, &dock);
    rail.bindDock(&secondSource, &secondDock);
    rail.show();
    dock.show();
    secondDock.show();
    application.processEvents();

    QToolButton* panelButton = nullptr;
    QToolButton* secondPanelButton = nullptr;
    QToolButton* commandButton = nullptr;
    QToolButton* moreButton = rail.findChild<QToolButton*>("NelsonPanelsMoreButton");
    int sourceButtonCount = 0;
    for (QToolButton* button : rail.findChildren<QToolButton*>()) {
        const bool nelsonRailButton = button->defaultAction() || button == moreButton;
        if (!nelsonRailButton) {
            continue;
        }
        if (button->defaultAction() && button->defaultAction()->text() == source.text()) {
            panelButton = button;
            sourceButtonCount++;
        } else if (button->defaultAction()
            && button->defaultAction()->text() == secondSource.text()) {
            secondPanelButton = button;
        } else if (button->defaultAction() && button->defaultAction()->text() == command.text()) {
            commandButton = button;
        }
        check(button->minimumSize().width() >= 32 && button->minimumSize().height() >= 32,
            "every rail tool button must provide at least a 32 px hit target");
        check(!button->accessibleName().trimmed().isEmpty(),
            "every rail tool button must expose an accessible name");
    }
    check(sourceButtonCount == 1, "the rail must expose each panel action only once");
    check(panelButton && panelButton->isChecked(), "a visible dock must mark its rail action");
    check(secondPanelButton && secondPanelButton->isChecked(),
        "each visible dock must mark its own rail action");
    check(commandButton != nullptr, "the rail must expose command actions as tool buttons");
    check(panelButton->accessibleName() == "Files",
        "rail panel buttons must expose an accessible name without mnemonics");
    check(moreButton && moreButton->menu(), "the rail must expose a More panels menu");
    check(moreButton->menu()->actions().contains(&source),
        "the More panels menu must include panel actions");
    check(moreButton->menu()->actions().contains(&secondSource),
        "the More panels menu must include every panel action");
    check(!moreButton->menu()->actions().contains(&command),
        "the More panels menu must not duplicate command actions");

    rail.setActiveDock(&dock);
    check(panelButton->property("activePanel").toBool(),
        "the focused dock must mark its rail action active");
    check(!secondPanelButton->property("activePanel").toBool(),
        "a visible but inactive dock must not be marked active");

    QAction* requestedAction = nullptr;
    int requestCount = 0;
    QObject::connect(&rail, &DesktopPanelRail::panelRequested, [&](QAction* action) {
        requestedAction = action;
        requestCount++;
    });
    secondPanelButton->defaultAction()->trigger();
    check(requestCount == 1 && requestedAction == &secondSource,
        "clicking a visible inactive rail panel must request that panel");
    check(secondPanelButton->isChecked(),
        "clicking a visible inactive panel must keep its visibility indicator checked");

    dock.hide();
    application.processEvents();
    check(!panelButton->isChecked(), "a hidden dock must clear its visibility state");
    check(!panelButton->property("activePanel").toBool(), "a hidden dock cannot remain active");
    check(secondPanelButton->isChecked(), "hiding one dock must not clear another visible dock");
}
//=============================================================================
static void
testShellHitTargets(QApplication& application)
{
    QMainWindow window;
    window.setObjectName("NelsonMainWindow");
    window.setStyleSheet(desktopShellStyleSheet(application.palette()));

    DesktopPanelRail* rail = new DesktopPanelRail(&window);
    QAction panel("Files", rail);
    panel.setCheckable(true);
    rail->addPanelAction(&panel);
    rail->complete();
    window.addToolBar(Qt::LeftToolBarArea, rail);

    QToolBar* toolbar = new QToolBar(&window);
    toolbar->setObjectName("NelsonMainToolBar");
    QAction toolbarAction("&Run", toolbar);
    toolbar->addAction(&toolbarAction);
    applyDesktopToolbarMetrics(toolbar);
    window.addToolBar(Qt::TopToolBarArea, toolbar);
    window.show();
    application.processEvents();
    applyDesktopToolbarMetrics(toolbar);

    QToolButton* panelButton = nullptr;
    for (QToolButton* button : rail->findChildren<QToolButton*>()) {
        if (button->defaultAction() && button->defaultAction()->text() == panel.text()) {
            panelButton = button;
            break;
        }
    }
    check(panelButton != nullptr, "the rail must expose a panel tool button");
    check(panelButton->minimumSize().width() >= 32 && panelButton->minimumSize().height() >= 32,
        "panel rail tool buttons must provide at least a 32 px hit target");

    QToolButton* toolbarButton = nullptr;
    for (QToolButton* button : toolbar->findChildren<QToolButton*>()) {
        if (button->defaultAction() == &toolbarAction) {
            toolbarButton = button;
            break;
        }
    }
    check(toolbarButton != nullptr, "the main toolbar must expose action tool buttons");
    check(toolbarButton->minimumSize().width() >= 32 && toolbarButton->minimumSize().height() >= 32,
        "main toolbar buttons must provide at least a 32 px hit target");
    check(toolbar->minimumHeight() == 38,
        "the main toolbar must use the common 38 px desktop height");
    check(toolbarButton->accessibleName() == "Run",
        "main toolbar buttons must expose an accessible name without mnemonics");
}
//=============================================================================
static void
testDesktopWindowChrome()
{
    QMainWindow window;
    QToolBar toolbar(&window);
    QAction action("Run", &toolbar);
    toolbar.addAction(&action);
    window.setWindowTitle(QStringLiteral("Nelson 2.0.0"));
    const Qt::WindowFlags initialFlags = window.windowFlags();

    applyDesktopWindowChrome(&window, &toolbar);

#ifdef Q_OS_MACOS
    check(window.windowTitle().isEmpty(),
        "the macOS unified toolbar must not be obscured by a centered window title");
    check(window.unifiedTitleAndToolBarOnMac(),
        "the macOS desktop must unify its title bar and main toolbar");
#if QT_VERSION >= QT_VERSION_CHECK(6, 9, 0)
    check(window.windowFlags().testFlag(Qt::ExpandedClientAreaHint)
            && window.windowFlags().testFlag(Qt::NoTitleBarBackgroundHint),
        "the macOS desktop must extend its toolbar into the title bar");
    check(!window.testAttribute(Qt::WA_ContentsMarginsRespectsSafeArea),
        "the macOS desktop must reclaim the title bar safe area");
    QWidget* spacer = toolbar.findChild<QWidget*>(
        QStringLiteral("NelsonMacTitleBarSpacer"), Qt::FindDirectChildrenOnly);
    check(spacer && spacer->width() == 72,
        "the macOS toolbar must reserve space for the window controls");
    check(toolbar.widgetForAction(toolbar.actions().first()) == spacer,
        "the macOS window-control spacer must precede toolbar actions");
#endif
#else
    check(window.windowTitle() == QStringLiteral("Nelson 2.0.0"),
        "desktop window chrome must preserve non-macOS window titles");
    check(window.windowFlags() == initialFlags,
        "desktop window chrome must not change non-macOS windows");
    check(toolbar.findChild<QWidget*>(
              QStringLiteral("NelsonMacTitleBarSpacer"), Qt::FindDirectChildrenOnly)
            == nullptr,
        "non-macOS toolbars must not reserve macOS window-control space");
#endif
}
//=============================================================================
static QMenu*
dockTitleMenu(QtDockTitleBar* titleBar)
{
    for (QToolButton* button : titleBar->findChildren<QToolButton*>()) {
        if (button->menu()) {
            return button->menu();
        }
    }
    return nullptr;
}
//=============================================================================
static QStringList
menuActionTexts(QMenu* menu)
{
    QStringList texts;
    for (QAction* action : menu->actions()) {
        if (!action->isSeparator()) {
            texts << action->text();
        }
    }
    return texts;
}
//=============================================================================
static void
rebuildDockMenu(QtDockTitleBar* titleBar)
{
    const bool invoked
        = QMetaObject::invokeMethod(titleBar, "rebuildActionsMenu", Qt::DirectConnection);
    check(invoked, "the dock title actions menu must be invokable for tests");
}
//=============================================================================
static void
testDockTitleMenu()
{
    QMainWindow window;
    QDockWidget commandDock("Command Window", &window);
    commandDock.setWidget(new QWidget(&commandDock));
    commandDock.setFeatures(QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetFloatable);
    QtDockTitleBar* commandTitle = new QtDockTitleBar(&commandDock, QString());
    commandDock.setTitleBarWidget(commandTitle);
    window.addDockWidget(Qt::LeftDockWidgetArea, &commandDock);

    rebuildDockMenu(commandTitle);
    QMenu* commandMenu = dockTitleMenu(commandTitle);
    check(commandMenu != nullptr, "a dock title bar must expose an actions menu");
    for (QToolButton* button : commandTitle->findChildren<QToolButton*>()) {
        check(button->minimumSize().width() >= 32 && button->minimumSize().height() >= 32,
            "dock title bar buttons must provide at least a 32 px hit target");
        check(!button->accessibleName().trimmed().isEmpty(),
            "dock title bar buttons must expose accessible names");
    }
    const QStringList commandActions = menuActionTexts(commandMenu);
    check(commandActions.contains("Collapse panel"),
        "a docked panel menu must include Collapse panel");
    check(commandActions.contains("Float panel"), "a floatable dock menu must include Float panel");
    check(commandActions.contains("Move Left") && commandActions.contains("Move Right")
            && commandActions.contains("Move Bottom"),
        "a movable dock menu must include direct move actions");
    check(!commandActions.contains("Close panel"),
        "a non-closable dock menu must not include Close panel");

    QDockWidget fixedDock("Fixed", &window);
    fixedDock.setWidget(new QWidget(&fixedDock));
    fixedDock.setFeatures(QDockWidget::DockWidgetFloatable);
    QtDockTitleBar* fixedTitle = new QtDockTitleBar(&fixedDock, QString());
    fixedDock.setTitleBarWidget(fixedTitle);
    window.addDockWidget(Qt::RightDockWidgetArea, &fixedDock);

    rebuildDockMenu(fixedTitle);
    QMenu* fixedMenu = dockTitleMenu(fixedTitle);
    check(fixedMenu != nullptr, "a non-movable dock title bar must expose an actions menu");
    const QStringList fixedActions = menuActionTexts(fixedMenu);
    check(!fixedActions.contains("Move Left") && !fixedActions.contains("Move Right")
            && !fixedActions.contains("Move Bottom"),
        "a non-movable dock menu must not include move actions");

    QDockWidget closableDock("Closable", &window);
    closableDock.setWidget(new QWidget(&closableDock));
    closableDock.setFeatures(QDockWidget::DockWidgetClosable | QDockWidget::DockWidgetMovable);
    QtDockTitleBar* closableTitle = new QtDockTitleBar(&closableDock, QString());
    closableDock.setTitleBarWidget(closableTitle);
    window.addDockWidget(Qt::BottomDockWidgetArea, &closableDock);

    rebuildDockMenu(closableTitle);
    QMenu* closableMenu = dockTitleMenu(closableTitle);
    check(closableMenu != nullptr, "a closable dock title bar must expose an actions menu");
    check(menuActionTexts(closableMenu).contains("Close panel"),
        "a closable dock menu must include Close panel");

    commandDock.setFloating(true);
    QApplication::processEvents();
    rebuildDockMenu(commandTitle);
    const QStringList floatingActions = menuActionTexts(commandMenu);
    check(!floatingActions.contains("Collapse panel"),
        "a floating dock menu must not include Collapse panel");
    check(floatingActions.contains("Dock panel"), "a floating dock menu must include Dock panel");
    check(!floatingActions.contains("Move Left") && !floatingActions.contains("Move Right")
            && !floatingActions.contains("Move Bottom"),
        "a floating dock menu must not include move actions");
}
//=============================================================================
int
main(int argc, char* argv[])
{
    if (qEnvironmentVariableIsEmpty("QT_QPA_PLATFORM")) {
        qputenv("QT_QPA_PLATFORM", "offscreen");
    }
    NelsonApplication application(argc, argv);
    testFileOpenEvents(application);
    testThemeTokens();
    testExamplesGalleryIntegration();
    testSvgTint();
    testNelsonShellSvgSet();
    testHighDpiIconVariants();
    testAccessibleActionText();
    testDesktopCommandShortcuts();
    testExclusiveActionSelectionCanBeCleared();
    testPaletteRanking();
    testCurrentDirectoryWidget();
    testRecentDirectoriesStore();
    testDialogDirectoriesStore();
    testApplicationWaitCursorPolicy();
    testPanelRailTransitions(application);
    testShellHitTargets(application);
    testDesktopWindowChrome();
    testDockTitleMenu();
    std::cout << "Desktop shell tests passed" << std::endl;
    return EXIT_SUCCESS;
}
//=============================================================================

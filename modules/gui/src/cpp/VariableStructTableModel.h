//=============================================================================
#pragma once
#include "Context.hpp"
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
#include "characters_encoding.hpp"
#include "VariableAbstractTableModel.h"
#include "QStringConverter.hpp"
#include "WorkspaceBrowser.hpp"
#include <QtCore/QAbstractTableModel>
#include <QtCore/QVector>
#include <QtCore/QString>
#include <QtCore/QVariant>
#include <QtCore/QTimer>
#include <QtCore/QSize>
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
class VariableStructTableModel : public VariableAbstractTableModel
{
    Q_OBJECT

public:
    explicit VariableStructTableModel(
        const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent = nullptr);
    ~VariableStructTableModel() override = default;

    Qt::ItemFlags
    flags(const QModelIndex& index) const override;
    bool
    setHeaderData(
        int section, Qt::Orientation orientation, const QVariant& value, int role) override;

    // QAbstractTableModel interface
    int
    rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int
    columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant
    data(const QModelIndex& index, int role) const override;
    bool
    setData(const QModelIndex& index, const QVariant& value, int role) override;
    QVariant
    headerData(int section, Qt::Orientation orientation, int role) const override;

    // Array management
    void
    refreshFromArray(ArrayOf& value) override;
    bool
    isStructureCompatible(const ArrayOf& value) override;
    void
    forceViewUpdate();

    QString
    getSelectedDataAsText(const QModelIndexList& selectedIndexes) const override;
    bool
    pasteDataFromExcel(int startRow, int startCol, const QString& text) override;
    bool
    pasteDataFromClipboard(int startRow, int startCol, const QString& text) override;

    bool
    canPasteAt(int startRow, int startCol, const QString& text) const;
    QSize
    getPasteSize(const QString& text) const override;

    ArrayOf
    createArrayFromSelection(const QModelIndexList& selectedIndexes) const override;
    bool
    isValidSelectionForExtraction(const QModelIndexList& selectedIndexes) const override;

    bool
    insertRowAt(int row) override;
    bool
    insertColumnAt(int col) override;
    bool
    deleteRowAt(int row) override;
    bool
    deleteColumnAt(int col) override;

    bool
    shouldDisplayAsTable() const;

    void
    undo() override;
    void
    redo() override;

    bool
    expandDisplayIfNeeded(int row, int col);

signals:
    void
    modelChanged();
    void
    errorOccurred(const QString& message);
    void
    selectionCreated(const QString& suggestedName, const ArrayOf& newArray);

private:
    // Core data management
    void
    initializeDimensions();
    void
    initializeEditData();

    // Display and editing
    QVariant
    getDisplayValue(int row, int col) const;
    bool
    hasOriginalData() const;
    bool
    isValidPosition(int row, int col) const;

    // Array manipulation
    bool
    resizeArrayIfNeeded(int row, int col);
    bool
    setArrayMissingValue(int row, int col);
    bool
    setArrayValue(int row, int col, const std::wstring& value);
    void
    updateEditData(int row, int col, const QVariant& value);

    void
    persistChangesToContext();
    void
    schedulePeristenceUpdate();
    void
    performBatchedPersistence();

    struct SelectionBounds
    {
        int minRow, maxRow, minCol, maxCol;
        bool isValid = false;
    };
    SelectionBounds
    getSelectionBounds(const QModelIndexList& selectedIndexes) const;

    // Enhanced paste helpers
    struct PasteData
    {
        QVector<QVector<QString>> cells;
        int rows = 0;
        int cols = 0;
        bool isValid = false;
    };
    PasteData
    parsePasteText(const QString& text) const;
    bool
    validatePasteData(const PasteData& data, int startRow, int startCol) const;
    bool
    applyPasteData(const PasteData& data, int startRow, int startCol);

    // Utilities
    int
    indexToFlat(int row, int col) const;
    std::pair<int, int>
    flatToIndex(int flatIndex) const;
    bool
    expandArrayIfNeeded(int row, int col);
    bool
    expandDisplay(int row, int col);
    bool
    resizeArray(int row, int col);
    void
    resizeEditData(int oldRows, int oldCols);

    // Member variables
    int m_rows = 0;
    int m_cols = 0;
    int m_rows_display = 0;
    int m_cols_display = 0;
    QVector<QVariant> m_editData;
    QTimer* m_persistenceTimer = nullptr;
    bool m_pendingChanges = false;

    QStringList m_fieldNames;

    // Configuration
    static constexpr int MIN_DISPLAY_SIZE = 10;
    static constexpr int DEFAULT_EMPTY_SIZE = 10;
    static constexpr int MAX_PASTE_SIZE = 10000;

    QVariant
    getCellDisplayValue(int row, int col) const;
    QVariant
    getCellEditValue(int row, int col) const;

    bool
    setCellValue(int row, int col, const ArrayOf& cellValue);
    QVariant
    formatStructForDisplay(const QVariant& cellData) const;
    QVariant
    formatStructForDisplay(const ArrayOf& cellArray) const;

    ArrayOf
    parseInputToArrayOf(const QString& input, int row, int col) const;
    QVariant
    formatCellForEdit(const ArrayOf& cellArray) const;

    QVariant
    formatCellForEdit(const QVariant& cellData) const;

    void
    updateFieldNames();

    bool
    setFieldValue(int structIndex, int fieldIndex, const ArrayOf& value);

    ArrayOf
    getFieldValue(int structIndex, int fieldIndex) const;

    QString
    getFieldName(int fieldIndex) const;
    int
    getFieldIndex(const QString& fieldName) const;

    bool
    isValidStructArray(const ArrayOf& array) const;

    bool
    hasConsistentFields(const ArrayOf& array) const;
};
//=============================================================================
} // namespace Nelson
//=============================================================================

//=============================================================================
#pragma once
//=============================================================================
#include "Context.hpp"
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
#include "characters_encoding.hpp"
#include "QStringConverter.hpp"
#include "WorkspaceBrowser.hpp"
#include <QtCore/QAbstractTableModel>
#include <QtCore/QVector>
#include <QtCore/QString>
#include <QtCore/QVariant>
#include <memory>
//=============================================================================
namespace Nelson {

class VariableTypeDefaultTableModel : public QAbstractTableModel
{
    Q_OBJECT

public:
    explicit VariableTypeDefaultTableModel(
        const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent = nullptr);
    ~VariableTypeDefaultTableModel() override = default;

    // QAbstractTableModel interface
    int
    rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int
    columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant
    data(const QModelIndex& index, int role) const override;
    bool
    setData(const QModelIndex& index, const QVariant& value, int role) override;
    Qt::ItemFlags
    flags(const QModelIndex& index) const override;
    QVariant
    headerData(int section, Qt::Orientation orientation, int role) const override;

    // Public interface
    QString
    variableName() const
    {
        return m_variableName;
    }
    const ArrayOf&
    array() const
    {
        return m_array;
    }
    void
    refreshFromArray();

    // Force view refresh
    void
    forceViewUpdate();

    // Enhanced paste functionality
    bool
    pasteData(int startRow, int startCol, const QString& text);
    bool
    canPasteAt(int startRow, int startCol, const QString& text) const;
    QSize
    getPasteSize(const QString& text) const;

signals:
    // Note: Removed custom dataChanged signal to avoid conflict with
    // QAbstractTableModel::dataChanged
    void
    modelChanged();
    void
    errorOccurred(const QString& message);

private:
    // Core data management
    void
    initializeDimensions();
    void
    initializeEditData();
    bool
    validateArrayState() const;

    // Display and editing
    QVariant
    getDisplayValue(int row, int col) const;
    bool
    hasOriginalData() const;
    bool
    isValidPosition(int row, int col) const;

    // Array manipulation
    bool
    expandDisplayIfNeeded(int row, int col);
    bool
    resizeArrayIfNeeded(int row, int col);
    bool
    setArrayValue(int row, int col, double value);
    bool
    setArrayValue(int row, int col, double realpart, double imagpart);

    void
    updateEditData(int row, int col, const QVariant& value);

    // Data conversion and validation
    bool
    parseNumericValue(const QString& text, double& value) const;
    QString
    formatDisplayValue(std::complex<double> value) const;
    QString
    formatDisplayValue(double value) const;

    // Persistence
    void
    persistChangesToContext();

    // Utilities
    int
    indexToFlat(int row, int col) const;
    std::pair<int, int>
    flatToIndex(int flatIndex) const;

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

    // Member variables
    ArrayOf m_array;
    Evaluator* m_evaluator;
    QString m_variableName;

    // Dimensions
    int m_rows = 0;
    int m_cols = 0;
    int m_rows_display = 0;
    int m_cols_display = 0;

    // Edit tracking
    QVector<QVariant> m_editData;

    // Configuration
    static constexpr int MIN_DISPLAY_SIZE = 10;
    static constexpr int DEFAULT_EMPTY_SIZE = 10;
    static constexpr int MAX_PASTE_SIZE = 10000; // Prevent excessive memory usage
};
//=============================================================================
} // namespace Nelson
//=============================================================================

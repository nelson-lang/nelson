//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <QtWidgets/QTreeWidget>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUITree.h"
#include "GOUITreeNode.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    static void
    appendNodeItems(GOUITree* owner, QTreeWidget* tree, QTreeWidgetItem* parentItem,
        GraphicsObject* nodeOwner, bool checkboxStyle, const std::vector<int64>& checkedHandles)
    {
        GOGObjectsProperty* childrenProperty = static_cast<GOGObjectsProperty*>(
            nodeOwner->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (!childrenProperty) {
            return;
        }
        for (int64 childHandle : childrenProperty->data()) {
            GOUITreeNode* node
                = dynamic_cast<GOUITreeNode*>(findGraphicsObject(childHandle, false));
            if (!node) {
                continue;
            }
            QTreeWidgetItem* item
                = parentItem ? new QTreeWidgetItem(parentItem) : new QTreeWidgetItem(tree);
            item->setText(0, wstringToQString(node->findStringProperty(GO_TEXT_PROPERTY_NAME_STR)));
            item->setData(0, Qt::UserRole, QVariant((qlonglong)childHandle));
            if (checkboxStyle) {
                item->setFlags(item->flags() | Qt::ItemIsUserCheckable | Qt::ItemIsAutoTristate);
                bool checked = std::find(checkedHandles.begin(), checkedHandles.end(), childHandle)
                    != checkedHandles.end();
                item->setCheckState(0, checked ? Qt::Checked : Qt::Unchecked);
            }
            appendNodeItems(owner, tree, item, node, checkboxStyle, checkedHandles);
        }
    }
    //=============================================================================
    static void
    collectItems(QTreeWidgetItem* item, std::vector<QTreeWidgetItem*>& list)
    {
        list.push_back(item);
        for (int k = 0; k < item->childCount(); ++k) {
            collectItems(item->child(k), list);
        }
    }
    //=============================================================================
    class UITreeBinding : public QtUIComponentBinding
    {
    public:
        using QtUIComponentBinding::QtUIComponentBinding;
        //=============================================================================
    protected:
        //=============================================================================
        void
        createWidget() override
        {
            QWidget* parent = getParentQWidget();
            if (!parent) {
                return;
            }
            QTreeWidget* tree = new QTreeWidget(parent);
            tree->setHeaderHidden(true);
            QObject::connect(
                tree, &QTreeWidget::itemSelectionChanged, tree, [this]() { onSelectionChanged(); });
            QObject::connect(tree, &QTreeWidget::itemExpanded, tree, [this](QTreeWidgetItem* item) {
                onNodeExpandCollapse(item, GO_NODE_EXPANDED_FCN_PROPERTY_NAME_STR,
                    L"NodeExpandedData", L"NodeExpanded");
            });
            QObject::connect(tree, &QTreeWidget::itemCollapsed, tree, [this](QTreeWidgetItem* item) {
                onNodeExpandCollapse(item, GO_NODE_COLLAPSED_FCN_PROPERTY_NAME_STR,
                    L"NodeCollapsedData", L"NodeCollapsed");
            });
            if (static_cast<GOUITree*>(model)->isCheckboxTree()) {
                QObject::connect(tree, &QTreeWidget::itemChanged, tree,
                    [this](QTreeWidgetItem*, int) { onItemChanged(); });
            }
            widget = tree;
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            GOUITree* goTree = static_cast<GOUITree*>(model);
            if (isNewWidget || model->hasChanged(GO_CHILDREN_PROPERTY_NAME_STR)
                || (goTree->isCheckboxTree()
                    && model->hasChanged(GO_CHECKED_NODES_PROPERTY_NAME_STR))) {
                rebuildTree();
            }
            QTreeWidget* tree = qobject_cast<QTreeWidget*>(widget.data());
            if (!tree) {
                return;
            }
            if (!goTree->isCheckboxTree()
                && (isNewWidget || model->hasChanged(GO_MULTISELECT_PROPERTY_NAME_STR))) {
                tree->setSelectionMode(
                    model->stringCheck(GO_MULTISELECT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)
                        ? QAbstractItemView::ExtendedSelection
                        : QAbstractItemView::SingleSelection);
            }
            int expandRequest = goTree->getPendingExpandCollapse();
            if (expandRequest > 0) {
                tree->expandAll();
            } else if (expandRequest < 0) {
                tree->collapseAll();
            }
        }
        //=============================================================================
    private:
        //=============================================================================
        void
        rebuildTree()
        {
            QTreeWidget* tree = qobject_cast<QTreeWidget*>(widget.data());
            if (!tree) {
                return;
            }
            GOUITree* goTree = static_cast<GOUITree*>(model);
            GOUIComponent::ProgrammaticGuard guard(model);
            std::vector<int64> checkedHandles;
            if (goTree->isCheckboxTree()) {
                GOGObjectsProperty* checkedNodes = static_cast<GOGObjectsProperty*>(
                    model->findProperty(GO_CHECKED_NODES_PROPERTY_NAME_STR, false));
                if (checkedNodes) {
                    checkedHandles = checkedNodes->data();
                }
            }
            tree->clear();
            appendNodeItems(
                goTree, tree, nullptr, goTree, goTree->isCheckboxTree(), checkedHandles);
            tree->expandAll();
        }
        //=============================================================================
        void
        onSelectionChanged()
        {
            if (model->isProgrammaticChange()) {
                return;
            }
            QTreeWidget* tree = qobject_cast<QTreeWidget*>(widget.data());
            if (!tree) {
                return;
            }
            GOGObjectsProperty* selectedNodes = static_cast<GOGObjectsProperty*>(
                model->findProperty(GO_SELECTED_NODES_PROPERTY_NAME_STR));
            std::vector<int64> previousSelection = selectedNodes->data();
            std::vector<int64> newSelection;
            for (QTreeWidgetItem* item : tree->selectedItems()) {
                newSelection.push_back((int64)item->data(0, Qt::UserRole).toLongLong());
            }
            selectedNodes->data(newSelection);

            wstringVector fieldNames = { L"SelectedNodes", L"PreviousSelectedNodes" };
            ArrayOfVector fieldValues;
            fieldValues << graphicsObjectVector(newSelection);
            fieldValues << graphicsObjectVector(previousSelection);
            model->emitCallbackEvent(GO_SELECTION_CHANGED_FCN_PROPERTY_NAME_STR,
                L"SelectedNodesChangedData", L"SelectionChanged", fieldNames, fieldValues);
        }
        //=============================================================================
        // Fires NodeExpandedFcn / NodeCollapsedFcn with the node that toggled.
        void
        onNodeExpandCollapse(QTreeWidgetItem* item, const std::wstring& callbackName,
            const std::wstring& eventDataClassName, const std::wstring& eventName)
        {
            if (!item || model->isProgrammaticChange()) {
                return;
            }
            int64 nodeHandle = (int64)item->data(0, Qt::UserRole).toLongLong();
            wstringVector fieldNames = { L"Node" };
            ArrayOfVector fieldValues;
            fieldValues << graphicsObjectHandle(nodeHandle);
            model->emitCallbackEvent(callbackName, eventDataClassName, eventName, fieldNames,
                fieldValues);
        }
        //=============================================================================
        void
        onItemChanged()
        {
            GOUITree* goTree = static_cast<GOUITree*>(model);
            if (model->isProgrammaticChange() || !goTree->isCheckboxTree()) {
                return;
            }
            QTreeWidget* tree = qobject_cast<QTreeWidget*>(widget.data());
            if (!tree) {
                return;
            }
            GOGObjectsProperty* checkedNodes = static_cast<GOGObjectsProperty*>(
                model->findProperty(GO_CHECKED_NODES_PROPERTY_NAME_STR));
            std::vector<int64> previousChecked = checkedNodes->data();
            std::vector<int64> newChecked;
            std::vector<QTreeWidgetItem*> items;
            for (int k = 0; k < tree->topLevelItemCount(); ++k) {
                collectItems(tree->topLevelItem(k), items);
            }
            for (QTreeWidgetItem* item : items) {
                if (item->checkState(0) == Qt::Checked) {
                    newChecked.push_back((int64)item->data(0, Qt::UserRole).toLongLong());
                }
            }
            checkedNodes->data(newChecked);

            wstringVector fieldNames = { L"CheckedNodes", L"PreviousCheckedNodes" };
            ArrayOfVector fieldValues;
            fieldValues << graphicsObjectVector(newChecked);
            fieldValues << graphicsObjectVector(previousChecked);
            model->emitCallbackEvent(GO_CHECKED_NODES_CHANGED_FCN_PROPERTY_NAME_STR,
                L"CheckedNodesChangedData", L"CheckedNodesChanged", fieldNames, fieldValues);
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUITreeBinding(GOUIComponent* component)
{
    return new UITreeBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================

//=============================================================================
// Delete audiorecorder handle helper
//=============================================================================
#include "DeleteAudiorecorderHandleObject.hpp"
#include "AudiorecorderObject.hpp"
#include "HandleManager.hpp"
#include "i18n.hpp"
#include "Error.hpp"

namespace Nelson {

bool
DeleteAudiorecorderHandleObject(const ArrayOf& A)
{
    bool res = false;
    if (A.isHandle()) {
        if (!A.isEmpty()) {
            Dimensions dims = A.getDimensions();
            nelson_handle* qp = (nelson_handle*)(A.getDataPointer());
            size_t elementCount = static_cast<size_t>(dims.getElementCount());
            for (size_t k = 0; k < elementCount; k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj) {
                    if (hlObj->getCategory() != NLS_HANDLE_AUDIORECORDER_CATEGORY_STR) {
                        Error(_W("audiorecorder handle expected."));
                    }
                    auto* recObj = (AudiorecorderObject*)hlObj;
                    delete recObj;
                    HandleManager::getInstance()->removeHandle(hl);
                    res = true;
                }
            }
        } else {
            Error(_W("audiorecorder valid handle expected."));
        }
    }
    return res;
}

} // namespace Nelson

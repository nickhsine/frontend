define([
    'common/utils/assign',
    'common/utils/closest',
    'common/utils/fastdom-promise',
    'commercial/modules/messenger'
], function (assign, closest, fastdom, messenger) {
    messenger.register('resize', function(specs, ret, iframe) {
        return resize(specs, iframe, closest(iframe, '.js-ad-slot'));
    });

    return resize;

    function resize(specs, iframe, adSlot) {
        if (!specs || !('height' in specs || 'width' in specs)) {
            return null;
        }

        var styles = {};

        if ('width' in specs) {
            styles.width = normalise(specs.width);
        }

        if ('height' in specs) {
            styles.height = normalise(specs.height);
        }

        return fastdom.write(function () {
            assign(adSlot.style, styles);
            assign(iframe.style, styles);
        });
    }

    function normalise(length) {
        var lengthRegexp = /^(\d+)(%|px|em|ex|ch|rem|vh|vw|vmin|vmax)?/;
        var defaultUnit = 'px';
        var matches = String(length).match(lengthRegexp);
        if (!matches) {
            return null;
        }
        return matches[1] + (matches[2] === undefined ? defaultUnit : matches[2]);
    }
});

define([
    'common/utils/fastdom-promise',
    'common/utils/$',
    'common/utils/detect',
    'common/utils/mediator',
    'common/utils/config',
    'common/utils/template',
    'common/views/svgs',
    'commercial/modules/creatives/gustyle',
    'text!commercial/views/creatives/gu-style-comcontent.html',
    'text!commercial/views/creatives/gu-style-hosted.html',
    'lodash/objects/merge',
    'commercial/modules/creatives/add-tracking-pixel'
], function (
    fastdom,
    $,
    detect,
    mediator,
    config,
    template,
    svgs,
    GuStyle,
    gustyleComcontentTpl,
    gustyleHostedTpl,
    merge,
    addTrackingPixel
) {

    var GustyleComcontent = function ($adSlot, params) {
        this.$adSlot = $adSlot;
        this.params  = params;
    };

    GustyleComcontent.prototype.create = function () {
        var externalLinkIcon = svgs('externalLink', ['gu-external-icon']),
            templateOptions = {
                articleContentColor: 'gu-display__content-color--' + this.params.articleContentColor,
                articleContentPosition: 'gu-display__content-position--' + this.params.articleContentPosition,
                articleHeaderFontSize: 'gu-display__content-size--' + this.params.articleHeaderFontSize,
                articleTextFontSize: 'gu-display__content-size--' + this.params.articleTextFontSize,
                brandLogoPosition: 'gu-display__logo-pos--' + this.params.brandLogoPosition,
                externalLinkIcon: externalLinkIcon,
                isHostedBottom: this.params.adType === 'gu-style-hosted-bottom'
            };
        var templateToLoad = this.params.adType === 'gu-style' ? gustyleComcontentTpl : gustyleHostedTpl;

        var title = this.params.articleHeaderText || 'unknown';
        var sponsor = 'Renault';
        this.params.linkTracking = 'Labs hosted native traffic card' +
            ' | ' + config.page.edition +
            ' | ' + config.page.section +
            ' | ' + title +
            ' | ' + sponsor;

        var markup = template(templateToLoad, { data: merge(this.params, templateOptions) });
        var gustyle = new GuStyle(this.$adSlot, this.params);

        return fastdom.write(function () {
            this.$adSlot[0].insertAdjacentHTML('beforeend', markup);

            if (this.params.trackingPixel) {
                addTrackingPixel(this.$adSlot, this.params.trackingPixel + this.params.cacheBuster);
            }
        }, this).then(gustyle.addLabel.bind(gustyle)).then(function () {
            return true;
        });
    };

    return GustyleComcontent;

});

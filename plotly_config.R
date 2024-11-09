modeBarButtonsToRemove <- c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale",
                            "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
                            "drawline", "drawopenpath", "drawclosedpath", "drawcircle", "drawrect", "eraseshape",
                            "zoom3d", "pan3d", "orbitRotation", "tableRotation", "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d",
                            "hoverClosestCartesian", "hoverCompareCartesian",
                            "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
                            "hoverClosestGl2d", "hoverClosestPie", "toggleHover", "resetViews", "sendDataToCloud", "toggleSpikelines", "resetViewMapbox")

plotly_config <- function(p) {
  p <- p %>%
    config(modeBarButtonsToRemove = modeBarButtonsToRemove, displaylogo = FALSE,
           toImageButtonOptions = list(format= 'svg',
                                       filename= 'custom_image',
                                       height= 900,
                                       width= 1400,
                                       scale= 1)) #%>%
    # layout()
}
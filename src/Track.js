"use strict";

// module App.Track

exports.encodeURIComponent = encodeURIComponent;

var React = require('react');
var ReactDOM = require('react-dom');
var Pux = require('purescript-pux');
var PropTypes = React.PropTypes;

var AudioPlayer = React.createClass({
  propTypes: {
    source: PropTypes.string.isRequired,
    isPlaying: PropTypes.bool,
    defaultTime: PropTypes.number,
    onProgress: React.PropTypes.func,
    onTimeUpdate: React.PropTypes.func,
    onEnd: React.PropTypes.func
  },
  componentDidMount: function() {
    var node = this.audioEl;

    node.addEventListener('progress', this.handleProgress);
    node.addEventListener('timeupdate', this.handleTimeUpdate);
    node.addEventListener('ended', this.handleMediaEnd);

    this.updateIsPlaying();
  },

  componentDidUpdate: function(prevProps) {
    if (prevProps.source !== this.props.source) {
      this.updateSource();
    }

    if (prevProps.isPlaying !== this.props.isPlaying) {
      this.updateIsPlaying();
    }

    if (prevProps.defaultTime !== this.props.defaultTime) {
      this.updateCurrentTime();
    }
  },

  componentWillUnmount: function() {
    var node = this.audioEl;

    node.removeEventListener('progress', this.handleProgress);
    node.removeEventListener('timeupdate', this.handleTimeUpdate);
    node.removeEventListener('ended', this.handleMediaEnd);
  },

  render: function() {
    var that = this;
    return (
      React.createElement("audio", {preload: "none", controls: true, ref: function (ref) { that.audioEl = ref; return ref;}},
                          React.createElement("source", {src: this.props.source + ".mp3"}),
                          React.createElement("source", {src: this.props.source + ".wav"})
                         )
    );
  },

  handleTimeUpdate: function() {
    var node = this.audioEl,
        currentTime = node.currentTime,
        trackDuration = node.duration;

    this.props.onTimeUpdate && this.props.onTimeUpdate({
      currentTime: currentTime,
      trackDuration: trackDuration
    });
  },

  handleMediaEnd: function() {
    this.audioEl.currentTime = 0;
    this.props.onEnd && this.props.onEnd();
  },

  handleProgress: function() {
    var node = this.audioEl,
        trackDuration = node.duration,
        buffered = node.buffered;

    this.props.onProgress && this.props.onProgress({
      trackDuration: trackDuration,
      buffered: buffered
    });
  },

  updateCurrentTime: function() {
    var node = this.audioEl;
    if (node.readyState) {
      node.currentTime = this.props.defaultTime;
    }
  },

  updateIsPlaying: function() {
    var node = this.audioEl,
        isPlaying = this.props.isPlaying;

    if (isPlaying) {
      node.play();
    } else {
      node.pause();
    }
  },

  updateSource: function() {
    var node = this.audioEl,
        isPlaying = this.props.isPlaying;

    node.pause();
    this.props.onTimeUpdate && this.props.onTimeUpdate({
      currentTime: 0,
      trackDuration: node.duration
    });

    node.load();
    if (isPlaying) {
      node.play();
    }
  }
});

exports.fromReact = Pux.fromReact(AudioPlayer);

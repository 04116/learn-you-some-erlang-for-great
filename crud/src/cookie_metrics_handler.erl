%% =============================================================================
%% Cookie CRUD API - Metrics HTTP Handler
%% =============================================================================
%% This module handles the /metrics endpoint for Prometheus scraping.
%% For Go developers, this is equivalent to:
%%   http.Handle("/metrics", promhttp.Handler())
%% =============================================================================

-module(cookie_metrics_handler).
-behaviour(cowboy_handler).

%% Cowboy handler callbacks
-export([init/2, terminate/3]).

%% =============================================================================
%% HTTP Handler Implementation
%% =============================================================================

%% Handle HTTP requests to /metrics endpoint
%% Go equivalent: func metricsHandler(w http.ResponseWriter, r *http.Request)
-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, State) ->
    %% Only allow GET requests
    Method = cowboy_req:method(Req),
    
    Req2 = case Method of
        <<"GET">> ->
            handle_metrics_get(Req);
        _ ->
            %% Return 405 Method Not Allowed for non-GET requests
            cowboy_req:reply(405, 
                #{<<"content-type">> => <<"text/plain">>},
                <<"Method Not Allowed">>, 
                Req)
    end,
    
    {ok, Req2, State}.

%% Cleanup callback (required by cowboy_handler behavior)
-spec terminate(term(), cowboy_req:req(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================

%% Handle GET /metrics request
%% Returns Prometheus-formatted metrics
-spec handle_metrics_get(cowboy_req:req()) -> cowboy_req:req().
handle_metrics_get(Req) ->
    %% Get metrics in Prometheus exposition format
    %% Go equivalent: prometheus.DefaultGatherer.Gather()
    MetricsData = cookie_metrics:get_metrics(),
    
    %% Set appropriate headers for Prometheus
    %% Content-Type must be text/plain for Prometheus compatibility
    Headers = #{
        <<"content-type">> => <<"text/plain; version=0.0.4; charset=utf-8">>
    },
    
    %% Return metrics response
    %% Go equivalent: w.Write(metricsData)
    cowboy_req:reply(200, Headers, MetricsData, Req).
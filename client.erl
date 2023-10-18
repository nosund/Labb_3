-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, { % client_st represent a state of a client
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    newchannel  % channels the client is connected to - WE ADDED
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{ 
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        newchannel = [] % We add a list for the channels that are created
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) -> % St is used to access and modify the state of the client
    case lists:member(St#client_st.server, registered()) of % Checks if the client's server is a member of the registered servers
        true -> Result = genserver:request(St#client_st.server, {join, Channel, self()}), % Sends a request to the server to join a specific channel, the client's Pid (self()) is included
            case Result of
                joined -> {reply, ok, St#client_st{newchannel = [Channel | St#client_st.newchannel]}}; % Request is accepted, updates the client's state to include the new joined channel in newchannel
                failed -> {reply, {error, user_already_joined, "client's join failed"}, St} % Server couldn't execute the join request, returns an error
            end;
        false -> {reply, {error, server_not_reached, "server not reached"}, St} % The client's server isn't registered it returns an error
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    case lists:member(Channel, St#client_st.newchannel) of % Checks if the specific channel is in the list of channels that the client has joined
        true -> genserver:request(list_to_atom(Channel), {leave, self()}), % Sends a request to the channel's process to leave the channel
            {reply, ok, St#client_st{newchannel = lists:delete(Channel, St#client_st.newchannel)}}; % Updates the client's state to remove the channel from the list of joined channels (newchannel)
        false -> {reply, {error, user_not_joined, "can't leave channe not joined"}, St} % Return an error  when client isn't a part of the specific channel
    end;

% Sending message (from GUI (graphical user interface), to channel)
handle(St, {message_send, Channel, Msg}) -> 
    % This  is where the meaasge sending is happening: request to the registered process (atom) which Channel is connected to
    % the tuple include info about the message including the channel, client's nickname, message content and client's Pid
    Result = genserver:request(list_to_atom(Channel), {message, Channel, St#client_st.nick, Msg, self()}), 
    case Result of % Checks if the message was successfully sent
        ok -> {reply, ok, St};
        failed -> {reply, {error, user_not_joined, "message not sent"}, St}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St};

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St};

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St};

% Catch-all for any unhandled requests
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.
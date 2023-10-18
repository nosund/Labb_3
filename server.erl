-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) -> % starts a new server with the given name
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, [], fun handlereq/2). % Starts the server and associate it with the handlereq function

handlereq(State, {join, Ch, Client}) -> % A client request to join a specific channel
    case lists:member(Ch, State) of % Checks if Ch (the specific channel) exists
        true -> Result = genserver:request(list_to_atom(Ch), {join, Client}), % Channel exist and the client is added to the channel
            case Result of
                joined -> {reply, joined, State}; % Updates State
                failed -> {reply, failed, State}
            end;
        false -> genserver:start(list_to_atom(Ch), [Client], fun newchannel/2),  % Channel doesn't exist, the server creates the channel with the client in it
            {reply, joined, [Ch | State]} % Client is added to the new channel, updates to State with the new channel included
    end;

handlereq(State, kill_channels) -> % Request to cancel/close a channel  
    lists:foreach(fun(Ch) -> genserver:stop(list_to_atom(Ch)) end, State), % 
    {reply, ok, []}.

newchannel(Clients, {join, Client}) -> % Client requests to join the channel
    case lists:member(Client, Clients) of % Checks  if Client is a member of the Clients list
        true -> {reply, failed, Clients}; % Client  is already in the channel, join request has failed, returns currents list Clients
        false -> {reply, joined, [Client | Clients]} % Client is not in the list, returns a new list with Client added to Clients
    end;

newchannel(Clients, {leave, Client}) -> % Client requests to leave the channel
    case lists:member(Client, Clients) of
        true -> Update_clients = lists:delete(Client, Clients), % Client is in Clients, removes Client from Clients list
            {reply, ok, Update_clients}; % Returns the updated list Update_clients
        false -> {reply, failed, [Clients]} % Client is not in list, leave request failed, returns the original list Clients
    end;

newchannel(Clients, {message, Channel, Nick, Msg, From}) -> % Function to send messages to all clients in a particular channel
    case lists:member(From, Clients) of % Checks if From (client's Pid) is a member of the Clients list
        % If From is in Clients, we create a new process which interates through the list and sends the message to each client in the list
        true -> spawn(fun() -> lists:foreach(fun(Pid) -> 
            if 
                Pid == From -> skip; % Checks if the current Pid in the list is the same as the sender's, in that case we skip sending the message to the sender itself
                true -> genserver:request(Pid, {message_recieve, Channel, Nick, Msg})
            end
        end,
        Clients) end),
        {reply, ok, Clients}; % If message is successfully sent, the function returns ok
        false -> {reply, failed, Clients} % If the sender isn't in Clients, the function returns failed
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, kill_channels),
    genserver:stop(ServerAtom).
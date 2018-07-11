(* This is a list of all CloudObject-related messages not defined in Messages/errmsg.m *)
(* Messages should ONLY be defined here TEMPORARILY until they are defined in the messages repo *)

BeginPackage["CloudObject`"];
Begin["`Private`"];

SetAttributes[addMessage, HoldFirst]
addMessage[mn_, text_] := If[!StringQ[mn], Set[mn,text]]

addMessage[CloudConnect::atype, "Unrecognized AuthenticationMethod specification `1`."];
addMessage[CloudConnect::aurl, "Invalid AuthenticationURL specification `1`."];
addMessage[CloudConnect::apkey, "Unrecognized authentication keys. Please contact technical support for assistance."];
addMessage[CloudConnect::badts, "Invalid Timestamp. Please ensure your system clock is set to the correct time."];
addMessage[CloudConnect::bdmtd, "HTTP method unavailable; Please contact technical support for assistance."];
addMessage[CloudConnect::bdrsp, "Unrecognized login response; Please contact technical support for assistance."];
addMessage[CloudConnect::cerr, "Unrecognized client error; status code `1`."];
addMessage[CloudConnect::clver, "Connecting to a cloud running an earlier version of the Wolfram Engine: `1`"];
addMessage[CloudConnect::config, "Unrecognized CloudConnect configuration."];
addMessage[CloudConnect::clver, "Connecting to a cloud running an earlier version of the Wolfram Engine: `1`"];
addMessage[CloudConnect::creds, "Incorrect username or password."];
addMessage[CloudConnect::fbdn, "Unable to authorize request.  Please contact technical support for assistance."];
addMessage[CloudConnect::gwto, "Unable to process request at this time. Please try again later."];
addMessage[CloudConnect::invcfg, "Unrecognized CloudConnect configuration."];
addMessage[CloudConnect::iserr, "Unable to process request. Please try again later."];
addMessage[CloudConnect::nfnd, "Unable to reach Wolfram Cloud servers. Please try again later."];
addMessage[CloudConnect::nocrd, "No username or password sent."];
addMessage[CloudConnect::notauth, "Unable to authenticate with Wolfram Cloud server. Please try authenticating again."];
addMessage[CloudConnect::oauth, "Unrecognized authentication information. Please contact technical support for assistance."];
addMessage[CloudConnect::pcond, "Invalid authorization information; Please contact technical support for assistance."];
addMessage[CloudConnect::serr, "Unrecognized server error; status code `1`."];
addMessage[CloudConnect::surl, "Invalid SignatureURL specification `1`."]
addMessage[CloudConnect::tout, "Request timed out. Please try again later."];
addMessage[CloudConnect::uerr, "An unknown error occurred; status code `1`."];
addMessage[CloudConnect::unav, "Wolfram Cloud temporarily unavailable. Please try again later."];
addMessage[CloudDeploy::invsrc, "Invalid source link `1`."];
addMessage[CloudLoggingData::inelem, "The argument `1` is not a valid logging data element."];
addMessage[CloudLoggingData::invobj, "The argument `1` is not a valid logging data object."];
addMessage[CloudLoggingData::invper, "The argument `1` is not a valid logging data period."];
addMessage[CloudLoggingData::invopt, "Value of \"TimeSeriesBinData->`1` is not a Quantity."];
addMessage[CloudObject::invbase, "Invalid CloudBase `1`; a fully qualified domain expected."];
addMessage[CloudObject::invuri, "The URI `1` is not valid."];
addMessage[CloudObject::invmeta, "Invalid meta information `1`; a list of rules or Association with string keys expected."];
addMessage[CloudObject::noicon, "No icon named `1` found for `2`."];
addMessage[CloudObject::unauth, "URI `1` only valid when authenticated."];
addMessage[CloudObject::uristring, "URI `1` expected to be a string."];
addMessage[CloudObjectInformation::noprop, "`1` is not a property returned by CloudObjectInformation."];
addMessage[CloudObjects::invtype, "Invalid cloud object type specification `1`."];
addMessage[CloudObjectNameFormat::una, "Unable to apply the specified name format `1`."];
addMessage[$CloudObjectNameFormat::inv, "CloudObjectNameFormat is not one of UUID, UserURLBase, CloudUserID, CloudUserUUID or Automatic."];
addMessage[ContinuousTask::restr, "Unrestricted cloud required for deployment."];
addMessage[DocumentGenerator::argu, "Unrecognized document generator specification."];
addMessage[DocumentGenerator::badarg, "Bad value `` for argument ``."]; 
addMessage[DocumentGenerator::badform, "Unrecognized output format ``."]; 
addMessage[DocumentGenerator::crea, "Unable to create or update generator."];
addMessage[DocumentGenerator::filex, "Cannot overwrite existing cloud object `1`."]; 
addMessage[DocumentGenerator::inact, "Document generator `1` is inactive."];
addMessage[DocumentGenerator::listing, "Unable to obtain DocumentGenerator listing."];
addMessage[DocumentGenerator::nffil, "`` not found."]; 
addMessage[DocumentGenerator::nochan, "Unsupported notification or delivery channel `1`."];
addMessage[DocumentGenerator::nonext, "Unable to obtain next scheduled run time for DocumentGenerator `1`."];
addMessage[DocumentGenerator::norm, "Unable to remove DocumentGenerator."];
addMessage[DocumentGenerator::nostart, "Unable to start task for document generator ``."];
addMessage[DocumentGenerator::nostop, "Unable to stop DocumentGenerator `1`."];
addMessage[DocumentGenerator::notrep, "Object `` not recognized as a document generator."];
addMessage[DocumentGenerator::notask, "No task found for object ``."];
addMessage[DocumentGenerator::optx, "Unknown option `1` in `2`."];
addMessage[DocumentGenerator::tcrea, "Unable to create generator task."];
addMessage[Export::argtu, "A format must be specified when exporting to a CloudObject."];
addMessage[General::cbase, "Invalid CloudBase specification `1`."];
addMessage[General::cloudnf, "No CloudObject found at the given address"]; (* we would like an argument: "No CloudObject at `1`."; but it requires some refactoring *)
addMessage[General::cloudprecondition, "A precondition check in the cloud service failed."];
addMessage[General::cloudunknown, "An unknown error occurred."];
addMessage[General::invcloudobj, "`1` is not a valid cloud object."];
addMessage[General::invperm, "Invalid permissions specification `1`."];
addMessage[General::invusr, "Invalid user or permissions group `1`."];  
addMessage[General::invkey, "The permissions key was not a non-empty string: `1`"];
addMessage[General::invcsk, "Invalid ConsumerKey of the SecuredAuthenticationKey : `1`"];
addMessage[General::keynf, "The PermissionsKey `1` was not found."];
addMessage[General::maxviewers, "Maximum number of `1` viewer seats exceeded."];
addMessage[General::noaccess, "Access to information for user `1` is denied."]; 
addMessage[General::notauth, "Unable to authenticate with Wolfram Cloud server. Please try authenticating again."];
addMessage[General::notmethod, "The specified method is not allowed."];
addMessage[General::notparam, "Invalid parameters were specified."];
addMessage[General::notperm, "Unable to perform the requested operation. Permission denied."];
addMessage[General::rejreq, "The specified request was rejected by the server."];(*rate limit exceeded, etc*)
addMessage[General::selfperm, "The currently authenticated user `1` cannot be assigned specific permissions. Owners always have full permissions on their objects."];
General::srverr = "Cloud server is not able to complete a request.";
addMessage[General::userunknown, "User `1` unknown to the Wolfram Cloud."];
addMessage[General::unavailable, "The cloud service is not available. Please try again shortly."];
addMessage[MailReceiverFunction::cloudc, "You must be cloud connected to deploy the MailReceiverFunction."];
addMessage[MailReceiverFunction::invfun, "A function is expected instead of `1`"];
addMessage[MailReceiverFunction::invmail, "The given message `1` is not valid mail."];
addMessage[MailResponseFunction::invopt, "The value `1` should be True, False, Automatic, or a function defining the desired response content."];
addMessage[MailReceiverFunction::nfile, "File `1` was not found."];
addMessage[MailReceiverFunction::nfrom, "The message should include a From parameter."];
addMessage[MailReceiverFunction::noco, "The MailReceiverFunction could not be deployed."];
addMessage[MailReceiverFunction::nomail, "The mbox file `1` does not include any messages."];
addMessage[MailReceiverFunction::perms, "MailReceiverFunction does not support the Permissions option."];
addMessage[PermissionsGroup::invperm, "Invalid permissions group `1`."];
addMessage[ReturnReceiptFunction::invopt, "The value `1` should be True, False, or a function determining whether to return a receipt."];
addMessage[RenameFile::cldnm, "Cloud object `1` does not contain a name."];
addMessage[ScheduledTask::ambig, "Timespec `` is ambiguous; try explicitly specifying start and end times."];
addMessage[ScheduledTask::argu, "Unrecognized scheduling specification."];
addMessage[ScheduledTask::badarg, "Bad value `1` for argument `2`."]; 
addMessage[ScheduledTask::copied, "Local file `1` copied to `2` for cloud execution."]; 
addMessage[ScheduledTask::crea, "Unable to create scheduled task."];
addMessage[ScheduledTask::inact, "Scheduled task `1` is inactive."];
addMessage[ScheduledTask::listing, "Unable to obtain ScheduledTask listing."];
addMessage[ScheduledTask::noavil, "Scheduling tasks remotely is not yet available."];
addMessage[ScheduledTask::nonext, "Unable to obtain next scheduled run time for ScheduledTask `1`."];
addMessage[ScheduledTask::norm, "Unable to remove ScheduledTask `1`."];
addMessage[ScheduledTask::norun, "Scheduled task `1` is not presently running."];
addMessage[ScheduledTask::nostart, "Unable to start ScheduledTask."];
addMessage[ScheduledTask::nostop, "Unable to stop ScheduledTask `1`."];
addMessage[ScheduledTask::notask, "Argument 1 in CloudDeploy is not a recognized ScheduledTask specification."];
addMessage[ScheduledTask::nouri, "Unrecognized uri specificiation `1`."];
addMessage[ScheduledTask::optx, "Unknown option `1` in `2`."];
addMessage[ScheduledTask::restr, "Use restricted under current subscription."];
addMessage[ScheduledTask::sched, "`1` is not a recognized scheduling time specification."];
addMessage[ScheduledTask::tasknf, "No task found at `1`."];
addMessage[ScheduledTask::upda, "Unable to update scheduled task."];
addMessage[ScheduledTask::unsuppsched, "The scheduling time specification `1` is not supported by the target environment."];

addMessage[HTTPResponse::encfailed, "Failed to encode HTTPResponse body"];
addMessage[HTTPResponse::nvldstatus, "Status code `` is not an integer from 200 to 511. 500 will be used."];
addMessage[HTTPResponse::nvldheaders, "Invalid headers specification ``. Using {}."];
addMessage[HTTPResponse::nvldbody, "CharacterEncoding can only be specified if the body is a string. None will be used."];
addMessage[HTTPResponse::nvldenc, "`` is not a supported character encoding. `` will be used."];
addMessage[HTTPResponse::chrmismatch, "The ContentType charset `` is not the same as the CharacterEncoding option ``."];
addMessage[HTTPResponse::nvldcttp, "Invalid ContentType ``. `` will be used."];
addMessage[HTTPResponse::nvldcache, "Invalid CachePersistence specification ``. `` will be used."];

addMessage[HTTPRequest::nvldmethod, "Invalid method `` specified for HTTPRequest. GET will be used."];
addMessage[HTTPRequest::chrmismatch, HTTPResponse::chrmismatch];
addMessage[HTTPRequest::nvldbody,    HTTPResponse::nvldbody];
addMessage[HTTPRequest::nvldenc,     HTTPResponse::nvldenc];
addMessage[HTTPRequest::encfailed,   "Failed to encode HTTPRequest body"];

addMessage[GenerateHTTPResponse::onlycloud, "`` is only supported in the Wolfram Cloud."];
addMessage[GenerateHTTPResponse::nvldrequest, "The second argument `` is not a valid Association, a list of rules or a URL."];


End[];
EndPackage[];
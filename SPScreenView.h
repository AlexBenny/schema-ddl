
//
// SPScreenView.h
// Snowplow
//
// Copyright (c) 2013-2021 Snowplow Analytics Ltd. All rights reserved.
//
// This program is licensed to you under the Apache License Version 2.0,
// and you may not use this file except in compliance with the Apache License
// Version 2.0. You may obtain a copy of the Apache License Version 2.0 at
// http://www.apache.org/licenses/LICENSE-2.0.
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the Apache License Version 2.0 is distributed on
// an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
// express or implied. See the Apache License Version 2.0 for the specific
// language governing permissions and limitations there under.
//
// Copyright: Copyright © 2021 Snowplow Analytics.
// License: Apache License Version 2.0
//

#import "SPEventBase.h"
#import "SPSelfDescribingJson.h"

NS_ASSUME_NONNULL_BEGIN

@interface SPScreenView : SPSelfDescribingAbstract
extern NSString * const kSPScreenViewSchema;

extern NSString * const kSPScreenViewParamPreviousType;

extern NSString * const kSPScreenViewParamName;

extern NSString * const kSPScreenViewParamPreviousName;

extern NSString * const kSPScreenViewParamTransitionType;

extern NSString * const kSPScreenViewParamId;

extern NSString * const kSPScreenViewParamType;

extern NSString * const kSPScreenViewParamPreviousId;



/// The screen type of the previous screenview.
@property (nonatomic, nonnull, readonly) NSString *previousType;

/// The name of the screen viewed.
@property (nonatomic, nonnull, readonly) NSString *name;

/// The name of the previous screen.
@property (nonatomic, nullable) NSString *previousName;

/// The type of transition that led to the screen being viewed.
@property (nonatomic, nonnull, readonly) NSString *transitionType;

/// An ID from the associated screenview event.
@property (nonatomic, nonnull, readonly) NSString *id;

/// The type of screen that was viewed e.g feed / carousel.
@property (nonatomic, nonnull, readonly) NSString *type;

/// A screenview ID of the previous screenview.
@property (nonatomic, nonnull, readonly) NSString *previousId;


- (instancetype)init NS_UNAVAILABLE;
- (instancetype)initWithPreviousType:(NSString *)previousType Name:(NSString *)name TransitionType:(NSString *)transitionType Id:(NSString *)id Type:(NSString *)type PreviousId:(NSString *)previousId ;


SP_BUILDER_DECLARE_NULLABLE(NSString *, previousName)


@end

NS_ASSUME_NONNULL_END
